-module(authentication_handler).

-export([
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    request_dispatcher/2,
    post_handler/2,
    allowed_methods/2
]).

init(Req, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, accept">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
	{cowboy_rest, Req3, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
        {<<"application/json">>, request_dispatcher}
    ], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, request_dispatcher}
	], Req, State}.

request_dispatcher(Req, State) -> 
    case cowboy_req:method(Req) of
        <<"POST">> ->
			post_handler(Req, State)
		end.

post_handler(Req, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req),
        #{<<"type">> := BinaryType, <<"user">> := BinaryUser, <<"password">> := BinaryPassword} = jsone:decode(Body),
        Type = binary_to_list(BinaryType),
        User = binary_to_list(BinaryUser),
        Password = binary_to_list(BinaryPassword),
        HashedPassword = crypto:hash(sha256, Password),
        if
            Type == "signup" ->
                {atomic, ok} = mnesia:transaction(fun() -> 
                    coin_node_mnesia:insert_new_user(User, HashedPassword, 0)    
                end);
            Type == "login" ->
                {atomic, {ok, StoredPassword}} = mnesia:transaction(fun() -> 
                    coin_node_mnesia:get_user_password(User)    
                end),
                true = StoredPassword == HashedPassword
        end,
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{<<"status">> => <<"success">>}), Req1),
        {true, Req2, State}
    catch
        error:_ ->
            Reply = jsone:encode(#{<<"status">> => <<"failed">>}),
            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Reply, Req),
            {halt, Req3, State}
    end.