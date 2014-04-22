-module(query_server).
-behavior(gen_server).

-record(state, {
								socket
							 }).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([connect/2]).
-compile([export_all]).


%%%%%%%%%%%%%%%
% API

test() ->
	Host = {66,150,155,158},
	Port = 2000,
	connect(Host, Port).

connect(Host, Port) ->
	gen_server:call(?MODULE, {connect, Host, Port}).

close() ->
	gen_server:call(?MODULE, close).
	


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("query SERVER: started~n"),
	{ok, #state{}}.


handle_call({connect, Host, Port}, _From, State) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, once}]),
	Msg = ok,
	{reply, Msg, State#state{socket=Socket}};
handle_call(close, _From, State) ->
	Msg = if State#state.socket =/= false ->
					gen_tcp:close(State#state.socket);
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

handle_info({tcp, _Socket, Msg}, State) ->
	io:format("received tcp message: ~p~n", [Msg]),
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
