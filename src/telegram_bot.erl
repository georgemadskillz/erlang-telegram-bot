-module(telegram_bot).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set_token/1]).
-export([get_me/0]).
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

set_token(Token) ->
    gen_server:call(?MODULE, {set_token, Token}).

get_me() ->
    gen_server:call(?MODULE, get_me).

get_updates() ->
    gen_server:call(?MODULE, get_updates).

%% Callbacks

init([]) ->
    application:ensure_all_started(gun),
    {ok, #{token => undefined}}.

handle_call({set_token, Token}, _From, State) ->
    {reply, ok, State#{token := Token}};
handle_call(get_me, _From, #{token := Token} = State) ->
    io:format("~p get_me call..~n", [self()]),
    {ok, Response} = request("/bot" ++ Token ++ "/getMe"),
    Decoded = decode(Response),
    {reply, {ok, Decoded}, State};
handle_call(get_updates, _From, #{token := Token} = State) ->
    io:format("~p get_updates call..~n", [self()]),
    {ok, Response} = request("/bot" ++ Token ++ "/getUpdates"),
    Decoded = decode(Response),
    {reply, {ok, Decoded}, State};
handle_call(Request, From, State) ->
    io:format("~p Unhandled call Request=~p, From=~p~n", [self(), Request, From]),
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

request(Request) ->
    {ok, ConnPid} = gun:open("api.telegram.org", 443, #{transport => tls}),
    io:format("~p Connect api.telegram.org..~n", [self()]),
    {ok, _Proto} = gun:await_up(ConnPid),
    io:format("~p Connected..~n", [self()]),

    Ref = gun:get(ConnPid, Request),
    {response, _, _, _} = gun:await(ConnPid, Ref),
    gun:await_body(ConnPid, Ref).
