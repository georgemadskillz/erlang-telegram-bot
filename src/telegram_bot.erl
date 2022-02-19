-module(telegram_bot).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([call_get/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {conn_pid}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call_get(Path) ->
    gen_server:call(telegram_bot, {get, Path}).

%% Callbacks

init([]) ->
    application:ensure_all_started(gun),
    {ok, #state{}}.

handle_call({get, Path}, _From, State) ->
    io:format("~p get -> Request=~p~n", [self(), Path]),

    {ok, ConnPid} = gun:open("api.telegram.org", 443, #{transport => tls}),
    io:format("~p get -> Opening connection with api.telegram.org, Pid=~p~n", [self(), ConnPid]),
    {ok, Proto} = gun:await_up(ConnPid),
    io:format("~p get -> Opened connection, Proto=~p~n", [self(), Proto]),

    Ref = gun:get(ConnPid, Path),
    Response = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    io:format("~p get -> Body=~p~n", [self(), Body]),

    {reply, {ok, Response}, State};
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
