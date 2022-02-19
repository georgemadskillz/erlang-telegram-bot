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
    TelegramBot = #{
        id => telegram_bot,
        start => {telegram_bot, start_link, []}
    },
    {ok, {SupFlags, [TelegramBot]}}.
