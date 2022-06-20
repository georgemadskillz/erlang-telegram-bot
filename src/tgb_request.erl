-module(tgb_request).

-export([send_get/1]).

send_get(Request) ->
    {ok, ConnPid} = gun:open(
        "api.telegram.org",
        443,
        #{
            transport => tls,
            transport_opts => [
                {verify, verify_none} % Suppress SSL cert absence warning
            ]
        }
    ),
    {ok, _Proto} = gun:await_up(ConnPid),
    Ref = gun:get(ConnPid, Request),
    {response, _, _, _} = gun:await(ConnPid, Ref),
    gun:await_body(ConnPid, Ref).
