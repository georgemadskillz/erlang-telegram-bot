-module(tgb_token_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set_token/1]).
-export([get_token/0]).

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

get_token() ->
    gen_server:call(?MODULE, get_token).

%% Callbacks

init([]) ->
    {ok, #{token => undefined}}.

handle_call({set_token, Token}, _From, State) ->
    {reply, ok, State#{token := Token}};
handle_call(get_token, _From, #{token := Token} = State) ->
    {reply, Token, State};
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
