%% Various utils functions
-module(cs_utils).
-author("Madalin Grigore-Enescu").

-include("cstasks.hrl").

-export([
    directory_sqlite/0, 
    directory_www/0,
    directory_release/0,
    directory_release/1
    ]).

%% @doc Return the configured path to the SQLite database or a path
%% to the release db/sqlite subdirectory
directory_sqlite() ->
    case application:get_env(?CS_APP_NAME, directory_sqlite) of
        {ok, Directory} -> directory_release(Directory);
        _ -> directory_release("db/sqlite")
    end.

%% @doc Return the configured path to the www or a path
%% to the release www subdirectory
directory_www() ->
    case application:get_env(?CS_APP_NAME, directory_www) of
        {ok, Directory} -> directory_release(Directory);
        _ -> directory_release("www")
    end.

%% @doc Return the configured path to the release or a path based on current module path
directory_release() ->
    case application:get_env(?CS_APP_NAME, directory_release) of
        {ok, Directory} -> Directory;
        _ -> 
            case code:which(?MODULE) of
                non_existing -> error(unknown_release_path);
                ModuleFilename ->
                    filename:dirname(filename:dirname(filename:dirname(filename:dirname(ModuleFilename))))
            end
    end.

%% @doc Return the specified path added to the release directory
directory_release(Path) -> 
    filename:join(directory_release(), Path).

    