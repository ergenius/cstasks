%% Record that holds all the information for a tasks job
%% I added id & title because initially i wanted to provide CRUD api operations
%% on jobs and insert the tasks into the sqlite database, but I overreach and I will probably 
%% don't have time to finish that. However I decided to keep the work already done
%% as an example of possible future improvments.
-record(cs_tasks_job, {
    id :: binary(),
    title :: binary(),
    unsorted_tasks :: [map()],
    sorted_tasks :: [map()],
    allow_multiple_sources :: boolean(),
    allow_disconnected :: boolean(),
    sort_time :: undefined | integer(),
    update_time :: integer()
}).

-type cs_tasks_job() :: #cs_tasks_job{}.

%% Header of the generated bash scripts
-define(CS_BASH_SCRIPTS_HEADER, <<"#!/usr/bin/env bash\n">>).

%% ETS memory tables
-define(CS_ETS_TABLE_CACHE_WWW, cs_table_cache_www).

%% ETS tables specifications
%% Tables are automatically created using this tables list
%% We can easily add a new table by appending the ETS table to this list
-define(CS_ETS_TABLES, [
    {?CS_ETS_TABLE_CACHE_WWW, [named_table, public]}
]).

-define(CS_APP_NAME, cstasks).

%% Can't live without my log macros :)
-define(CS_LOG_LOCATION, #{
    mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
    line => ?LINE,
    file => ?FILE
}).
-define(CS_LOG(Level, Report), logger:log(Level, Report, ?CS_LOG_LOCATION)).
-define(CS_LOG_EMERGENCY(A), ?CS_LOG(emergency, A)).
-define(CS_LOG_ALERT(A), ?CS_LOG(alert, A)).
-define(CS_LOG_CRITICAL(A), ?CS_LOG(critical, A)).
-define(CS_LOG_ERROR(A), ?CS_LOG(error, A)).
-define(CS_LOG_WARNING(A), ?CS_LOG(warning, A)).
-define(CS_LOG_NOTICE(A), ?CS_LOG(notice, A)).
-define(CS_LOG_INFO(A), ?CS_LOG(info, A)).
-ifdef(CS_DEBUG).
-define(CS_LOG_DEBUG(A), ?CS_LOG(debug, A)).
-else.
-define(CS_LOG_DEBUG(A), ok).
-endif.