-module(tgb_updater).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_updates/0]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_updates() ->
    gen_server:call(?MODULE, get_updates).

%% Callbacks

init([]) ->
    {ok, _} = timer:apply_interval(1000, ?MODULE, get_updates, []),
    {ok, #{}}.

handle_call(get_updates, _From, State) ->
    case tgb_token_keeper:get_token() of
        undefined ->
            %io:format("~p Cannot perform action, token is undefined!~n", [?MODULE]),
            {reply, {error, {token, undefined}}, State};
        Token ->
            {ok, Response} = get_request("/bot" ++ Token ++ "/getUpdates"),
            Decoded = decode(Response),
            tgb_server:handle_updates(Decoded),
            {reply, ok, State}
    end;
handle_call(Request, From, State) ->
    io:format("~p Unhandled call Request=~p, From=~p~n", [?MODULE, Request, From]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

decode(Json) ->
    jsx:decode(Json, []).

get_request(Request) ->
    {ok, ConnPid} = gun:open("api.telegram.org", 443, #{transport => tls}),
    io:format("~p Connect api.telegram.org..~n", [?MODULE]),
    {ok, _Proto} = gun:await_up(ConnPid),
    io:format("~p Connected..~n", [?MODULE]),

    Ref = gun:get(ConnPid, Request),
    {response, _, _, _} = gun:await(ConnPid, Ref),
    gun:await_body(ConnPid, Ref).
