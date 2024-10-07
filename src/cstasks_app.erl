-module(cstasks_app).
-behaviour(application).

-include("cstasks.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

%% @doc application start callback
start(_Type, _Args) ->
    %% Start required application
    {ok, _} = application:ensure_all_started(kernel),
    {ok, _} = application:ensure_all_started(public_key),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(esqlite),

    %% Create ets used all over the place
    [ets:new(TableName, TableSpecs) || {TableName, TableSpecs} <- ?CS_ETS_TABLES],

    %% Start HTTP listener
    {ok, _} = start_http_listener(),

    %% Start application main supervisor
    case cs_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

%% @doc Start all server protocols
start_http_listener() ->
    Port = application:get_env(cstasks, cowboy_http_port, 1986),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/[...]", cs_http_handler_api, #{}},
            {'_', cs_http_handler_www, #{}}
        ]}
    ]),
    cowboy:start_clear(cs_cowboy_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

%% @doc application stop callback.
stop(_State) ->
    cowboy:stop_listener(cs_cowboy_http_listener),
    ok.