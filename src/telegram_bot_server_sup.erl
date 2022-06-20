-module(telegram_bot_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, _} = application:ensure_all_started(gun),
    case application:get_env(telegram_bot_list) of
        undefined ->
            throw(telegram_bot_list_undefined);
        {ok, BotConfigList} ->
            SupFlags = #{
                strategy => one_for_one,
                intensity => 1,
                preiod => 5,
                auto_shutdown => never
            },
            Childs = make_childs(BotConfigList),
            {ok, {SupFlags, Childs}}
    end.

make_childs([]) ->
    throw(telegram_bot_list_empty);
make_childs(BotConfigList) ->
    make_childs(BotConfigList, []).

make_childs([], Childs) ->
    Childs;
make_childs([BotConfig|Rest], Childs) ->
    Child = make_child(BotConfig),
    make_childs(Rest, [Child|Childs]).

make_child(#{name := BotName, api_token := Token}) when is_binary(Token) ->
    #{
        id => BotName,
        start => {tgb_server, start_link, [#{api_token => Token, name => BotName}]}
    }.
