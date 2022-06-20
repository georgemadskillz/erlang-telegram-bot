
-module(tgb_database).

% API

-export([read_data/2]).
-export([write_data/3]).

% API

read_data(Key, TableName) ->
    case open_table(TableName) of
        {ok, Table} ->
            case dets:lookup(Table, Key) of
                [] ->
                    dets:close(Table),
                    undefined;
                [{Key, Data}] ->
                    dets:close(Table),
                    Data
            end;
        {error, Error} ->
            {error, Error}
    end.

write_data(Key, Data, TableName) ->
    case open_table(TableName) of
        {ok, Table} ->
            ok = dets:insert(Table, {Key, Data}),
            ok = dets:close(Table);
        Error ->
            {error, Error}
    end.


% Internal

open_table(TableName) ->
    filelib:ensure_dir("database/"),
    Path = "database/db_" ++ atom_to_list(TableName),
    case dets:open_file(tgbdb, [{type, set}, {file, Path}]) of
        {ok, Table} ->
            {ok, Table};
        Error ->
            Error
    end.
