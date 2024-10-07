%% This is the sorting solution based on Kahn algorithm.
%%
%% How I end up chosing Kahn?
%%
%% - It was obviously this is a graph sorting problem.
%% - It is also obvious that the input data must be a Directed Acyclic Graph (a graph with no cycles) because otherwise
%% we will not be able to solve it and we will end up with circular dependencies and infinite loops.
%% - The first algorithm that I could think of was the famous Depth First Search
%% - Searching more information on this subject I came across Kahn algorithm and I found it more interesting
%% to exercise it.
%% - When chosing in between DFS & Kahn I also took into consideration both the time & space complexity
%% and they are both the same - O(V+E). So from this point of view I thought both could do the job.
%% - When studying various Kahn examples something "contrived" struck me to use the word from the coding challenge:
%% many implementations examples are based on data that keep track of the CHILDREN dependencies not the FATHER
%% nodes/vertices (our require). This was form me one of the tricky aspect when solving this problem.
%%
%% - After writing the basic algorithm I also start thinking to some corner case scenarios and
%% implemented them as options or errors.
%% - In a normal relation with a client I would ask the client about those corner case
%% scenarios but because I had to finish the test in weekend I decided not to bother Vivien from CS
%% with extra questions and simply implement all those corner case scenarios as API options.
%%
%% It is also worth mentioning I favored eficiency. Because of this I prefered to mix and implement
%% most validation inside the sort itself in order to avoid extra iterations of the tasks
%% list outside the sort algorithm.
%%
%% The corner case scenarios implemented as options are:
%% - should we allow or not more than 1 source vertice?
%% - should we allow or not disconected vertices?
%%
%% The error (validation) case scenarios are:
%% - invalid or malformed tasks with missing or invalid names, commands and requires
%% - we must detect cycles (tasks that are linked to each other in a loop)
%% and we must obviously consider such an input invalid by returning a specific error
%% - we must return an error when duplicate task names are encountered
-module(cs_sort_kahn).
-author("Madalin Grigore-Enescu").

-behaiviour(cs_sort_behaiviour).
-include("cstasks.hrl").

-export([sort/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SORT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Sort the tasks using Kahn's Algorithm
sort(
    Job = #cs_tasks_job{
        unsorted_tasks = UnsortedTasks,
        allow_multiple_sources = AllowMultipleSources,
        allow_disconnected = AllowDisconnected
    }
) ->
    ?CS_LOG_DEBUG(#{sort_job => Job}),

    SortTimeStart = erlang:system_time(microsecond),

    %% Build the graph adjacency list and indegree map
    BuildGraphResult = build_graph(UnsortedTasks),

    ?CS_LOG_DEBUG(#{build_graph => BuildGraphResult}),
    %% BuildGraphResult sample:
    %% {ok,#{<<"task-1">> => [<<"task-3">>],
    %%                   <<"task-2">> => [<<"task-4">>],
    %%                   <<"task-3">> => [<<"task-4">>,<<"task-2">>]},
    %%                 #{<<"task-1">> => 0,<<"task-2">> => 1,<<"task-3">> => 1,
    %%                   <<"task-4">> => 2}}

    case BuildGraphResult of
        {ok, AdjList, IndegreeMap, CommandMap} ->

            %% Find tasks with no dependencies
            %% (tasks with indegree = 0)
            Queue = find_indegree_zero_tasks(IndegreeMap),

            ?CS_LOG_DEBUG(#{find_indegree_zero_tasks => Queue}),

            %% Check if multiple source vertices are allowed
            %% (more than one task with indegree = 0)
            MultipleSources = check_multiple_sources(AllowMultipleSources, Queue),
            ?CS_LOG_DEBUG(#{check_multiple_sources => MultipleSources}),
            case MultipleSources of
                ok ->
                    %% Check for disconnected vertices
                    DisconnectedVertices = check_disconnected_vertices(
                        UnsortedTasks, AdjList, IndegreeMap, AllowDisconnected
                    ),
                    ?CS_LOG_DEBUG(#{disconnected_vertices => DisconnectedVertices}),
                    case DisconnectedVertices of
                        ok ->
                            %% TOPOLOGICAL SEARCH LOOP
                            UnsortedTasksLength = length(UnsortedTasks),
                            case
                                topological_sort(
                                    Queue, AdjList, IndegreeMap, UnsortedTasksLength
                                )
                            of
                                {ok, SortedNames} ->
                                    SortedTasks = build_sorted_task_list(SortedNames, CommandMap),
                                    SortTimeEnd = erlang:system_time(microsecond),                                    
                                    NewJob = Job#cs_tasks_job{
                                        sorted_tasks = SortedTasks,
                                        sort_time = SortTimeEnd - SortTimeStart
                                    },
                                    ?CS_LOG_DEBUG(#{sorted_job => NewJob}),
                                    {ok, NewJob};
                                %% {error, cycle_detected}
                                Error3 ->
                                    Error3
                            end;
                        %% {error, disconnected_vertices_not_allowed}
                        Error2 ->
                            Error2
                    end;
                %% {error, no_source_vertice} or {error, multiple_sources_not_allowed}
                Error1 ->
                    Error1
            end;
        Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build_graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Build the adjacency list and indegree map using recursion
build_graph([]) ->
    {error, empty_tasks_list};
build_graph(Tasks) ->
    build_graph(Tasks, #{}, #{}, #{}).

build_graph([], AdjList, IndegreeMap, CommandMap) ->
    {ok, AdjList, IndegreeMap, CommandMap};
build_graph([Task | Rest], AdjList, IndegreeMap, CommandMap) ->
    case build_graph_validate_task(Task) of
        {ok, Name, Command, Requires} ->
            %% Add the task to indegree map
            %% Most Kahn algorithms examples I could find are implemented assuming
            %% the inverse relation father -> child in the input data.
            %% Our input is the other way around, each vertice specifing the required tasks
            %% - the fathers not the children. So at this point we know exactly the indegree
            %% for this task = erlang:length(Requires) and there is no need to count.
            UpdatedIndegreeMap = maps:put(Name, erlang:length(Requires), IndegreeMap),
            %% Update the adjacency list
            %% Again, remember we have the input data the other way around compared
            %% to the direction we will explore the graph so for each task this task depends on
            %% add this task as child node...
            UpdatedAdjList = build_graph_update_adjacency_list(Requires, Name, AdjList),
            %% Build CommandsMap (faster than locating later the sorted tasks command in the input list)
            NewCommandMap = maps:put(Name, Command, CommandMap),
            build_graph(Rest, UpdatedAdjList, UpdatedIndegreeMap, NewCommandMap);
        Error ->
            Error
    end.

%% @doc Carefully validate each task and return task elements in the same step
build_graph_validate_task(Task) ->
    Name = maps:get(<<"name">>, Task, undefined),
    Command = maps:get(<<"command">>, Task, undefined),
    Requires = maps:get(<<"requires">>, Task, undefined),
    build_graph_validate_task(Name, Command, Requires).

%% @doc Validate the specified task components
build_graph_validate_task(<<>>, _Command, _Requires) ->
    {error, invalid_task_empty_name};
build_graph_validate_task(Name, _Command, _Requires) when not is_binary(Name) ->
    {error, invalid_task_name};
%% I decided not to allow empty commands despite they can't hurth much
%% this could be a warning that something is wrong with the input data.
build_graph_validate_task(_Name, <<>>, _Requires) ->
    {error, invalid_task_empty_command};
build_graph_validate_task(_Name, Command, _Requires) when not is_binary(Command) ->
    {error, invalid_task_command};
%% Allow undefined requires
build_graph_validate_task(Name, Command, undefined) ->
    {ok, Name, Command, []};
%% Validate requires
build_graph_validate_task(Name, Command, Requires) when is_list(Requires) ->
    build_graph_validate_task_requires(Name, Command, Requires, Requires);
%% Invalid requires
build_graph_validate_task(_Name, _Command, _Requires) ->
    {error, invalid_task_requires}.

%% @doc Validate each element of the requires list
build_graph_validate_task_requires(Name, Command, Requires, []) ->
    {ok, Name, Command, Requires};
build_graph_validate_task_requires(Name, Command, Requires, [H | T]) when
    is_binary(H), H =/= <<>>
->
    build_graph_validate_task_requires(Name, Command, Requires, T);
build_graph_validate_task_requires(_Name, _Command, _Requires, _) ->
    {error, invalid_task_requires}.

%% @doc Recursively update the adjacency list for dependencies
build_graph_update_adjacency_list([], _TaskName, AdjList) ->
    AdjList;
build_graph_update_adjacency_list([RequiredName | Requires], TaskName, AdjList) ->
    %% Update a value in a Map1 associated with Key by calling Fun on the old value to get a new value.
    %% If Key is not present in Map1 then Init will be associated with Key.
    UpdatedAdjList = maps:update_with(
        RequiredName, fun(OldValue) -> [TaskName | OldValue] end, [TaskName], AdjList
    ),
    build_graph_update_adjacency_list(Requires, TaskName, UpdatedAdjList).

%% @doc Find all tasks with indegree 0 (no dependencies)
find_indegree_zero_tasks(IndegreeMap) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case Value of
                0 -> [Key | Acc];
                _ -> Acc
            end
        end,
        [],
        IndegreeMap
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHECK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Check source vertices to see if the initial queue contains multiple source vertices
%% or not and it it is allowed.
check_multiple_sources(_, []) ->
    %% Graph without at least one source vertice - it is obviously invalid
    %% because we don't know where to start and we can't just chose a random
    %% bash command. This corner case scenario is also detected later
    %% when cecking for cycles - but it's faster to detect this from the begining...
    {error, no_source_vertice};
check_multiple_sources(_, [_]) ->
    ok;
check_multiple_sources(true, _Queue) ->
    ok;
check_multiple_sources(false, _Queue) ->
    {error, multiple_sources_not_allowed}.

%% @doc Check for disconnected vertices
check_disconnected_vertices(_Tasks, _AdjList, _IndegreeMap, true) ->
    ok;
check_disconnected_vertices(Tasks, AdjList, IndegreeMap, false) ->
    case check_disconnected_vertices_find(Tasks, AdjList, IndegreeMap) of
        [] -> ok;
        _ -> {error, disconnected_vertices_not_allowed}
    end.

%% @doc Find disconnected vertices - tasks not in adjacency list with indegree 0
check_disconnected_vertices_find(Tasks, AdjList, IndegreeMap) ->
    lists:filter(
        fun(Task) ->
            TaskName = maps:get(<<"name">>, Task),
            maps:is_key(TaskName, AdjList) =:= false andalso
                maps:get(TaskName, IndegreeMap) =:= 0
        end,
        Tasks
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% topological_sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Topological sort with cycle detection
%% At the end after all tasks are iterated we check if the number of tasks processed
%% is diffrent from the total tasks count.
topological_sort(Queue, AdjList, IndegreeMap, TotalTasks) ->
    topological_sort(Queue, AdjList, IndegreeMap, [], TotalTasks, 0).

%% @doc This is the main search iteration
%% End?
topological_sort([], _, _, Sorted, TotalTasks, ProcessedCount) ->
    if
        ProcessedCount =:= TotalTasks ->
            {ok, Sorted};
        true ->
            {error, cycle_detected}
    end;
%% Sort baby!
topological_sort(
    [Task | Queue], AdjList, IndegreeMap, Sorted, TotalTasks, ProcessedCount
) ->
    %% Get the list of tasks that require the current task
    Children = maps:get(Task, AdjList, []),
    %% update the indegree of children
    {UpdatedQueue, UpdatedIndegreeMap} = process_children(Children, Queue, IndegreeMap),
    topological_sort(
        UpdatedQueue,
        AdjList,
        UpdatedIndegreeMap,
        %% REMEMBER to revert this list at the end!
        [Task | Sorted],
        TotalTasks,
        ProcessedCount + 1
    ).

%% Process the children of the specified task
process_children([], Queue, IndegreeMap) ->
    {Queue, IndegreeMap};
process_children([Child | Rest], Queue, IndegreeMap) ->
    UpdatedIndegree = maps:get(Child, IndegreeMap) - 1,
    UpdatedIndegreeMap = maps:put(Child, UpdatedIndegree, IndegreeMap),
    %% Add child to the queue if neccessary
    UpdatedQueue =
        if
            UpdatedIndegree =:= 0 -> [Child | Queue];
            true -> Queue
        end,
    process_children(Rest, UpdatedQueue, UpdatedIndegreeMap).

%% @doc Build the sorted task list from the topologically sorted names 
%% and command map
build_sorted_task_list(SortedNames, CommandMap) -> build_sorted_task_list(SortedNames, CommandMap, []).
build_sorted_task_list([Name|T], CommandMap, Acum) ->
    %% Missing map element here is a strong indication of a bug
    %% Let it crush!
    Command = maps:get(Name, CommandMap),
    SortedTask = #{
        <<"name">> => Name,
        <<"command">> => Command
    },
    build_sorted_task_list(T, CommandMap, [SortedTask | Acum]);
build_sorted_task_list([], _CommandMap, Acum) -> Acum.
