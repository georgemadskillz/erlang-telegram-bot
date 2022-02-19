-module(client).

-compile(export_all).

http_get(Path) ->
    {ok, Response} = telegram_bot:call_get(Path),
    io:format("~p client -> Response=~p~n", [self(), Response]).
