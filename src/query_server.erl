-module(query_server).
-behavior(gen_server).

-record(state, {
								port,
								host,
								socket
							 }).

-include("include/binary.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([connect/2]).
-compile([export_all]).


%%%%%%%%%%%%%%%
% API

test() ->
	Host = {66,150,155,158},
	Port = 2000,
	ok = connect(Host, Port),
	info().

connect(Host, Port) ->
	gen_server:call(?MODULE, {connect, Host, Port}).

player() ->
	gen_server:call(?MODULE, player).

info() ->
	gen_server:call(?MODULE, info).

close() ->
	gen_server:call(?MODULE, close).
	


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("query SERVER: started~n"),
	{ok, #state{}}.


handle_call({connect, Host, Port}, _From, State) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
	{reply, ok, State#state{socket=Socket, host=Host, port=Port}};
handle_call(player, _From, State) ->
	Req = [<<16#FFFFFFFF?L>>, <<16#55?B>>, <<16#FFFFFFFF?L>>],
	Val = gen_udp:send(State#state.socket, Req),
	io:format("sending : ~p~n", [Req]),
	io:format("udp send val: ~p~n", [Val]),
	Msg = ok,
	{reply, Msg, State};
handle_call(info, _From, State = #state{socket=Socket, host=Host, port=Port}) ->
	Req = [<<16#FFFFFFFF?L>>, <<16#54?B>>, <<"Source Engine Query">>, <<0?B>>],
	ok = gen_udp:send(Socket, Host, Port, Req),
	io:format("sending info request~n"),
	Msg = ok,
	{reply, Msg, State};
handle_call(close, _From, State) ->
	Msg = if State#state.socket =/= false ->
					gen_udp:close(State#state.socket);
				true ->
					ok
				end,
	{reply, Msg, State#state{socket=false}};
handle_call(Msg, _From, State) ->
	io:format("received unknown call message: ~p~n", [Msg]),
	{noreply, State}.

handle_cast(Msg, State) ->
	io:format("received unknown cast message: ~p~n", [Msg]),
	{noreply, State}.

handle_info({udp, _Socket, _SrcHost,  _SrcPort, <<Bin/binary>>}, State) ->
	parse_info_response(Bin),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("received unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	io:format("code change~n"),
	{ok, State}.

terminate(normal, _State) ->
	ok;
terminate(_Reason, _State) ->
	ok.



parse_info_response(Bin) ->
	List = binary_to_list(Bin),
	io:format("List: ~p~n", [List]),
	Fields = [{request_header, long}, {header, byte}, {protocol, byte}, {name, string}, {map, string}, {folder, string}, {game, string}, {id, short}, {players, byte}, {max_players, byte}, {bots, byte}, {server_type, char}, {environment, char}, {visibility, byte}, {vac, byte}, {version, string}, {edf, byte}],
	Data = parse_protocol(Fields, List),
	io:format("~p~n", [Data]),
	ok.
	%S = "game name: ~p~nserver name: ~p~nmap: ~p~nplayers: ~p/~p with ~p bots~ntype: ~p~nenv: ~p~nvis: ~p~nvac~p~n",
	%List = [GameName, SrvName, Map, NumPlayers, MaxPlayers, NumBots, SrvType, Env, Visibility, Vac],
	%io:format(S, List).

test_parse() ->
	List = [255,255,255,255,73,17,1,1,1,1,1,1,1,1,1,1,1,1,1,32,32,32,32,115,107,
105,97,108,46,99,111,109,32,124,32,66,65,68,87,65,84,69,82,32,124,32,
76,65,0,112,108,95,98,97,100,119,97,116,101,114,0,116,102,0,84,101,97,
109,32,70,111,114,116,114,101,115,115,0,184,1,24,24,0,100,108,0,1,50,
49,55,55,57,48,50,0,177,208,7,44,3,0,0,0,0,48,1,72,76,115,116,97,116,
115,88,58,67,69,44,83,107,105,97,108,44,83,107,105,97,108,46,99,111,
109,44,95,114,101,103,105,115,116,101,114,101,100,44,97,108,108,116,97,
108,107,44,112,97,121,108,111,97,100,0,184,1,0,0,0,0,0,0],
	Fields = [{request_header, long}, {header, byte}, {protocol, byte}, {name, string}, {map, string}, {folder, string}, {game, string}, {id, short}, {players, byte}, {max_players, byte}, {bots, byte}, {server_type, char}, {environment, char}, {visibility, byte}, {vac, byte}, {version, string}],
	Data = parse_protocol(Fields, List),
	io:format("full data: ~p~n", [Data]).

test_p1() ->
	_String = [115,107,
105,97,108,46,99,111,109,32,124,32,66,65,68,87,65,84,69,82,32,124,32,
76,65,0],
	_StringFields = [{name, string}],
	_Long = [255, 255, 255, 255],
	_LongFields = [{long, long}],
	_Short = [184, 1],
	_ShortFields = [{short, short}],
	Byte = [85],
	ByteFields = [{byte, byte}],
	Data = parse_protocol(ByteFields, Byte),
	io:format("simple data: ~p~n", [Data]).

parse_protocol([], Rest) ->
	io:format("rest: ~p~n", [Rest]),
	[];
parse_protocol([{Name, Type}|Rest], List) ->
	{Data, NewList} = case Type of
											string ->
												String = getString(List),
												String2 = lists:filter(fun(Elem) -> Elem > 1 end, String),
												N = length(String) + 1,
												Tail = lists:nthtail(N, List),
												{String2, Tail};
											long ->
												E1 = hd(List),
												E2 = hd(tl(List)),
												E3 = hd(tl(tl(List))),
												E4 = hd(tl(tl(tl(List)))),
												<<Val?L>> = <<E1?B, E2?B, E3?B, E4?B>>,
												N = 4,
												Tail = lists:nthtail(N, List),
												{Val, Tail};
											short ->
												N = 2,
												E1 = hd(List),
												E2 = hd(tl(List)),
												<<Val?S>> = <<E1?B, E2?B>>,
												Tail = lists:nthtail(N, List),
												{Val, Tail};
											byte ->
												N = 1,
												Val = hd(List),
												Tail = lists:nthtail(N, List),
												{Val, Tail};
											char ->
												N = 1,
												Val = hd(List),
												Tail = lists:nthtail(N, List),
												{[Val], Tail}
										end,
	[{Name, Data}] ++ parse_protocol(Rest, NewList).

getString(List) ->
	{_, String} = lists:foldl(fun(Elem, {Bool, S}) ->
									if Bool == true ->
											{Bool, S};
										true ->
											if Elem == 0 ->
													{true, S};
												true ->
													{false, [Elem|S]}
											end
										end
								end, {false, []}, List),
	lists:reverse(String).
	
		%_ReqHeader?L,
		%_Header?B,
		%_Protocol?B,
		%SrvName,
		%16#0?B,
		%Map,
		%16#0?B,
		%_Folder,
		%16#0?B,
		%GameName,
		%16#0?B,
		%_Id?S,
		%NumPlayers?B,
		%MaxPlayers?B,
		%NumBots?B,
		%SrvType?B,
		%Env?B,
		%Visibility?B,
		%Vac?B
