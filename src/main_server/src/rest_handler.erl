-module(rest_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([get_handler/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, get_handler}
	], Req, State}.

get_handler(Req, State) ->
    #{coin := Coin} = cowboy_req:match_qs([{coin, nonempty}], Req),
    Reply = jsone:encode(#{<<"coin">> => Coin}),
	{Reply, Req, State}.