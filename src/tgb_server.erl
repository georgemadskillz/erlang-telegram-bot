-module(tgb_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([timer_callback/0]).
-export([handle_updates/1]).

%% Callbacks
-export([init/1]).
-export([handle_continue/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% API

start_link(BotConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, BotConfig, []).

timer_callback() ->
    gen_server:call(?MODULE, timer).

handle_updates(Updates) ->
    gen_server:call(?MODULE, {updates, Updates}).

%% Callbacks

init(BotConfig) ->
    io:format("~p>~p started..~n", [?MODULE, self()]),
    {ok, #{}, {continue, {init, BotConfig}}}.

handle_continue({init, #{api_token := Token, name := BotName}}, State) ->
    % TODO: sysconfig validation
    LastUpdateID = read_lats_update_id(BotName),
    io:format("~p>~p Init> LastUpdateID=~p~n", [?MODULE, self(), LastUpdateID]),
    GetTokenFun = fun() -> Token end,
    io:format("~p>~p Init> Token OK~n", [?MODULE, self()]),
    {ok, _} = timer:apply_interval(3000, ?MODULE, timer_callback, []),
    {noreply, State#{last_update_id => LastUpdateID, get_token_fun => GetTokenFun, name => BotName}}.

handle_call(timer, _From, #{get_token_fun := GetTokenFun, last_update_id := LastID, name := BotName} = State) ->
    Token = GetTokenFun(),
    {ok, Updates} = get_updates(LastID, Token),
    io:format("~p>~p Got Updates=~p~n", [?MODULE, self(), Updates]),
    case handle_updates(Updates, LastID) of
        {ok, NewLastID} ->
            update_last_update_id(NewLastID, BotName),
            io:format("~p>~p Updates processed, the last with ID=~p~n", [?MODULE, self(), NewLastID]),
            {reply, ok, State#{last_update_id := NewLastID}};
        error ->
            {reply, ok, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

% Internal functions

get_updates(LastID, Token) ->
    StringToken = binary_to_list(Token),
    Request = case LastID of
        undefined ->
            io:format("~p>~p Last UpdatesID undefined..~n", [?MODULE, self()]),
            "/bot" ++ StringToken ++ "/getUpdates?";
        ID ->
            RequestID = ID + 1,
            StringID = integer_to_list(RequestID),
            io:format("~p>~p Last UpdatesID=~p, request Offset=~p~n", [?MODULE, self(), ID, RequestID]),
            "/bot" ++ StringToken ++ "/getUpdates?offset=" ++ StringID
    end,
    tgb_request:send_get(Request).

read_lats_update_id(BotName) ->
    tgb_database:read_data(last_update_id, BotName).

update_last_update_id(ID, BotName) ->
    ok = tgb_database:write_data(last_update_id, ID, BotName).

handle_updates(Updates, LastID) ->
    handle_updates(decode, Updates, LastID).

handle_updates(decode, Updates, LastID) ->
    case try_decode_updates(Updates) of
        error ->
            io:format("~p>~p Failed to decode raw updates into JSON!~n", [?MODULE, self()]),
            error;
        DecodedUpdates ->
            handle_updates(get_updates_list, DecodedUpdates, LastID)
    end;
handle_updates(get_updates_list, Updates, LastID) ->
    case Updates of
        #{<<"ok">> := true, <<"result">> := UpdatesList} ->
            handle_updates(updates_list, UpdatesList, LastID);
        _Other ->
            io:format("~p>~p Bad updates format!~n", [?MODULE, self()]),
            error
    end;
handle_updates(updates_list, UpdatesList, LastID) ->
    handle_updates_list(UpdatesList, LastID).

try_decode_updates(RawUpdates) ->
    try
        jsx:decode(RawUpdates, [])
    catch
        _:_ ->
            error
    end.

handle_updates_list([], LastID) ->
    {ok, LastID};
handle_updates_list([Update|Rest], LastID) ->
    case handle_update(Update) of
        {ok, NewLastID} ->
            handle_updates_list(Rest, NewLastID);
        {error, ErrorID} ->
            io:format("~p>~p Update#~p handling error, skip!~n", [?MODULE, self(), ErrorID]),
            handle_updates_list(Rest, LastID)
    end.

handle_update(#{<<"update_id">> := NewID} = Update) when is_integer(NewID) ->
    handle_update(Update, NewID);
handle_update(_Other) ->
    {error, unknown}.

handle_update(#{<<"message">> := _Message}, NewID) ->
    io:format("~p>~p Got update#~p~n", [?MODULE, self(), NewID]),
    %parse_message(Message),
    {ok, NewID};
handle_update(_Other, NewID) ->
    io:format("~p>~p Unhandled update data with ID=~p~n", [?MODULE, self(), NewID]),
    {error, NewID}.

