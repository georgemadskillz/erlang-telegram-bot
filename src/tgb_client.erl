-module(tgb_client).

-compile(export_all).

set_token(Token) ->
    ok = tgb_token_keeper:set_token(Token).

get_me() ->
    Result = tgb_server:get_me(),
    io:format("~p bot_get_me -> Response=~p~n", [?MODULE, Result]).
