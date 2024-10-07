%% Contains varios helper functions for dealing with a tasks job record
-module(cs_tasks_job).
-author("Madalin Grigore-Enescu").

-include("cstasks.hrl").

-export([new_from_request/1]).
-export([serialize_to_map/1]).
-export([serialize_to_bash/1]).

%% @doc Create a new tasks job from the specified cowboy request
%% I decided to preserve as much popssible of the original data format comming from Cowboy request
%% in order to avoid performance penalties that comes from converting data around to other types.
new_from_request(Req) ->
    {ok, Data, Req1} = cowboy_req:read_body(
        Req,
        #{
            length => application:get_env(?CS_APP_NAME, cowboy_read_body_length, 20000000),
            period => application:get_env(?CS_APP_NAME, cowboy_read_body_period, 120000)
        }
    ),
    DecodedBody = cs_json:decode(Data),
    ?CS_LOG_DEBUG(DecodedBody),
    {ok, new_from_map(DecodedBody), Req1}.

%% @doc Create a new tasks job from the specified map
new_from_map(MapJob) when erlang:is_map(MapJob) ->
    new(
        maps:get(<<"id">>, MapJob, undefined),
        maps:get(<<"title">>, MapJob, undefined),
        maps:get(<<"tasks">>, MapJob, undefined),
        undefined,
        maps:get(<<"allow_multiple_sources">>, MapJob, false),
        maps:get(<<"allow_disconnected">>, MapJob, false),
        undefined
    ).

%% @doc Create a new tasks job
new(Id, Title, UnsortedTasks, SortedTasks, AllowMultipleSources, AllowDisconnected, UpdateTime) ->
    #cs_tasks_job{
        id = new_id(Id),
        title = new_title(Title),
        unsorted_tasks = new_unsorted_tasks(UnsortedTasks),
        sorted_tasks = new_sorted_tasks(SortedTasks),
        allow_multiple_sources = new_allow_multiple_sources(AllowMultipleSources),
        allow_disconnected = new_allow_disconnected(AllowDisconnected),
        update_time = new_update_time(UpdateTime)
    }.

new_id(Id) when is_binary(Id), Id =/= <<>> ->
    Id;
new_id(_Id) ->
    cs_uuid:new().
new_title(Title) when is_binary(Title) ->
    Title;
new_title(_Title) ->
    <<>>.
new_unsorted_tasks(UnsortedTasks) when is_list(UnsortedTasks) ->
    UnsortedTasks;
new_unsorted_tasks(_UnsortedTasks) ->
    [].
new_sorted_tasks(SortedTasks) when is_list(SortedTasks) ->
    SortedTasks;
new_sorted_tasks(_SortedTasks) ->
    [].

new_allow_multiple_sources(Bool) when is_boolean(Bool) -> Bool;
new_allow_multiple_sources(_Bool) -> false.

new_allow_disconnected(Bool) when is_boolean(Bool) -> Bool;
new_allow_disconnected(_Bool) -> false.

new_update_time(UpdateTime) when is_integer(UpdateTime) -> UpdateTime;
new_update_time(_UpdateTime) -> erlang:system_time(millisecond).

%% @doc Serialize sorted tasks to map
serialize_to_map(#cs_tasks_job{
        sorted_tasks = SortedTasks,
        sort_time = SortTime
    }) ->
    #{
        <<"tasks">> => SortedTasks,
        <<"sort_time_milliseconds">> => SortTime
    }.

%% @doc Serialize sorted tasks to bash binary
serialize_to_bash(#cs_tasks_job{
        sorted_tasks = SortedTasks
    }) ->
    serialize_to_bash_iterate(SortedTasks, [?CS_BASH_SCRIPTS_HEADER]).
serialize_to_bash_iterate([H|T], Acum) ->
    Command = maps:get(<<"command">>, H, <<>>),
    serialize_to_bash_iterate(T, [<<"\n">> | [Command | Acum]]);
serialize_to_bash_iterate([], Acum) ->
    erlang:iolist_to_binary(lists:reverse(Acum)).
            