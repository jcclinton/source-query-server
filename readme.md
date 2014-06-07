# Erlang Source Query

## Description
An as-yet unfinished Erlang module that queries game servers which use the Source query protocol. Protocol can be found here https://developer.valvesoftware.com/wiki/Server_queries

## Status
Currently you can query the basic server information (AS2_INFO). This is just a proof of concept, there is not much functionality implemented.


## Example
```erlang
application:start(source_query_server).
query_server:connect({66,150,155,158}, 2000).
query_server:info(). %prints out info automatically to io
query_server:close().
application:stop(source_query_server).
```
