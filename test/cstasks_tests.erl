-module(cstasks_tests).
-include_lib("eunit/include/eunit.hrl").
-include("cstasks.hrl").

%% Tests
-define(CS_TESTS, [
    {
        ok,
        [
            #{
                <<"command">> => <<"touch /tmp/file1">>,
                <<"name">> => <<"task-1">>
            },
            #{
                <<"command">> => <<"cat /tmp/file1">>,
                <<"name">> => <<"task-2">>,
                <<"requires">> => [<<"task-3">>]
            },
            #{
                <<"command">> =>
                    <<"echo 'Hello World!' > /tmp/file1">>,
                <<"name">> => <<"task-3">>,
                <<"requires">> => [<<"task-1">>]
            },
            #{
                <<"command">> => <<"rm /tmp/file1">>,
                <<"name">> => <<"task-4">>,
                <<"requires">> => [<<"task-2">>, <<"task-3">>]
            }
        ],
        false,
        false
    },
    {
        ok,
        [
            #{
                <<"command">> => <<"echo \"Allow multiple sources\"">>,
                <<"name">> => <<"task-0">>
            },
            #{
                <<"command">> => <<"touch /tmp/file1">>,
                <<"name">> => <<"task-1">>
            },
            #{
                <<"command">> => <<"cat /tmp/file1">>,
                <<"name">> => <<"task-2">>,
                <<"requires">> => [<<"task-3">>]
            },
            #{
                <<"command">> =>
                    <<"echo 'Hello World!' > /tmp/file1">>,
                <<"name">> => <<"task-3">>,
                <<"requires">> => [<<"task-1">>]
            },
            #{
                <<"command">> => <<"rm /tmp/file1">>,
                <<"name">> => <<"task-4">>,
                <<"requires">> => [<<"task-2">>, <<"task-3">>]
            }
        ],
        true,
        true
    },
    {
        fail,
        [
            #{
                <<"command">> => <<"touch /tmp/file1">>
            }
        ],
        false,
        false
    },
    {
        fail,
        [
            #{
                <<"name">> => <<"task-4">>
            }
        ],
        false,
        false
    },
    {
        fail,
        [
            #{
                <<"name">> => <<"task-1">>,
                <<"requires">> => [<<"task-1">>]
            }
        ],
        false,
        false
    },
    {
        fail,
        [
            #{
                <<"name">> => <<"task-1">>,
                <<"requires">> => [<<"task-2">>]
            }
        ],
        false,
        false
    }
]).

create_job(UnsortedTasks, AllowMultipleSources, AllowDisconnected) ->
    #cs_tasks_job{
        id = <<"2ca67016621a7d6b591e218fe1">>,
        title = <<>>,
        unsorted_tasks = UnsortedTasks,
        sorted_tasks = [],
        allow_multiple_sources = AllowMultipleSources,
        allow_disconnected = AllowDisconnected,
        sort_time = undefined,
        update_time = erlang:system_time(millisecond)
    }.

%% A more compact way of writing tests (and much more flexible, as we shall see),
%% is to write functions that return tests, instead of being tests.
%% See here: https://www.erlang.org/doc/apps/eunit/chapter#writing-test-generating-functions
sort_test_() -> sort_test(?CS_TESTS, []).
sort_test([], Acum) ->
    Acum;
sort_test([{Type, UnsortedTasks, AllowMultipleSources, AllowDisconnected} | T], Acum) ->
    Job = create_job(UnsortedTasks, AllowMultipleSources, AllowDisconnected),
    Result = sort_test_check(Type, Job),
    sort_test(T, [Result | Acum]).

sort_test_check(ok, Job) -> ?_test(?assertMatch({ok, _}, cs_sort_kahn:sort(Job)));
sort_test_check(fail, Job) -> ?_test(?assertMatch({error, _}, cs_sort_kahn:sort(Job))).
