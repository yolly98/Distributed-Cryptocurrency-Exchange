-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).


init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 60000}}.

websocket_init(State) ->
	global:register_name({ws, node(), self()}, self()),
	io:format("registered pid ~p\n", [self()]),
 	{[], State}.

websocket_handle({text, Msg}, State) ->
	DecodedMsg = jsone:try_decode(Msg),
	case element(1, DecodedMsg) of
		ok ->
			Json = element(2, DecodedMsg),
			{ok, Opcode} = maps:find(<<"opcode">>, Json),
			case Opcode of
				<<"keepalive">> ->
					Reply = jsone:encode(#{<<"opcode">> => <<"keepalive">>})
			end,
			{[{text, Reply}], State};
		error ->
			Reply = jsone:encode(#{<<"opcode">> => <<"error">>, <<"message">> => <<"invalid json">>}),
			{[{text, Reply}], State}
	end.

websocket_info({broadcast, Msg}, State) ->
    {[{text, Msg}], State};

websocket_info({timeout, _Ref, Msg}, State) ->
  	{[{text, Msg}], State};

websocket_info(_Info, State) ->
  	{[], State}.

websocket_terminate(_Reason, _Req, _State) ->
    % unregister the connection
	io:format("websocket closed [~p]\n", [self()]),
    global:unregister_name({ws, node(), self()}),
    ok.