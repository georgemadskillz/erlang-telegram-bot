-module(client).

-compile(export_all).

set_token(Token) ->
    ok = telegram_bot:set_token(Token).

get_me() ->
    {ok, Response} = telegram_bot:get_me(),
    io:format("~p bot_get_me -> Response=~p~n", [self(), Response]).

get_updates() ->
    {ok, Response} = telegram_bot:get_updates(),
    io:format("~p bot_get_updates -> Response=~p~n", [self(), Response]).
