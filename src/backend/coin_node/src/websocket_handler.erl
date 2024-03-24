-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).


init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 60000}}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),  % TEST
	global:register_name({ws, node(), self()}, self()),
	io:format("registered pid ~p\n", [self()]), % TEST
 	{[], State}.

websocket_handle({text, Msg}, State) ->
	% decode Msg
	Decoded_msg = jsone:try_decode(Msg),
	case element(1, Decoded_msg) of
		ok ->
			Json = element(2, Decoded_msg),
			{ok, Opcode} = maps:find(<<"opcode">>, Json),
			{ok, Name} = maps:find(<<"name">>, Json),
			case Opcode of
				<<"new">> ->
					{ok, Surname} = maps:find(<<"surname">>, Json),
					mnesia_test:createNewPerson(Name, Surname),
					Reply = jsone:encode(#{<<"status">> => <<"ok">>});
				<<"delete">> ->
					mnesia_test:deletePerson(Name),
					Reply = jsone:encode(#{<<"status">> => <<"ok">>});
				<<"select">> ->
					{_, {_, People}} = mnesia_test:selectPersonByName(Name),
					Reply = jsone:encode(#{<<"result">> => jsone:encode(People)})
			end,
			{[{text, Reply}], State};
		error ->
			{[{text, "Invalid Json"}], State}
	end.

websocket_info({broadcast, Msg}, State) ->
    {[{text, Msg}], State};

websocket_info({timeout, _Ref, Msg}, State) ->
  	% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  	{[{text, Msg}], State};
websocket_info(_Info, State) ->
  	{[], State}.

websocket_terminate(_Reason, _Req, _State) ->
    % Unregister the connection
	io:format("websocket closed [~p]\n", [self()]),
    global:unregister_name({ws, node(), self()}),
    ok.