%%%-------------------------------------------------------------------
%% @doc telegram_bot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(telegram_bot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        preiod => 5,
        auto_shutdown => never
    },
    Server = #{
        id => tgb_server,
        start => {tgb_server, start_link, []}
    },
    Updater = #{
        id => tgb_updater,
        start => {tgb_updater, start_link, []}
    },
    TokenKeeper = #{
        id => tgb_token_keeper,
        start => {tgb_token_keeper, start_link, []}
    },
    {ok, {SupFlags, [Server, Updater, TokenKeeper]}}.
