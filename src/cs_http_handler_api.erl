%% API Cowboy HTTP handler
-module(cs_http_handler_api).
-author("Madalin Grigore-Enescu").

-include("cstasks.hrl").

-export([init/2]).
-export([terminate/3]).

%% @doc Init callback
init(Req, State) ->
    case filter(Req, State) of
        {ok, Result, NewReq} ->
            {ok, reply_ok(Result, NewReq), State};
        Error ->
            {ok, reply_error(Error, Req), State}
    end.

%% @doc Terminate callback.
%%
%% This callback is strictly reserved for any required cleanup. You cannot send a response from this function. There is no other return value.
%% If you used the process dictionary, timers, monitors or may be receiving messages, then you can use this function to clean them up, as
%% Cowboy might reuse the process for the next keep-alive request.
%% Note that while this function may be called in a Websocket handler, it is generally not useful to do any clean up as the process terminates
%% immediately after calling this callback when using Websocket.
terminate(_Reason, _Req, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Filter the request
filter(Req, State) ->
    Method = cowboy_req:method(Req),
    filter_path(Method, Req, State).

%% @doc Filter path
%% We can have a nice system with separate modules for each API path
%% but for now this is enough. I added the version to show an example of versioning
%% the API using url.
filter_path(Method, Req = #{path := Path}, State) -> 
    filter_path(Path, Method, Req, State).

%% Sort and return JSON
filter_path(<<"/api/v1/sort/json">>, <<"POST">>, Req, State) ->
    api_sort_json(Req, State);
filter_path(<<"/api/v1/sort/json">>, _, _Req, _State) ->
    {error, bad_request};

%% Sort and return bash script
filter_path(<<"/api/v1/sort/bash">>, <<"POST">>, Req, State) ->
    api_sort_bash(Req, State);
filter_path(<<"/api/v1/sort/bash">>, _, _Req, _State) ->
    {error, bad_request};

%% I couldn't finish those CRUD operations, but i live them here as an example of
%% what i intended to use the sqlite database for...

%% Update a specific job
%% filter_path(<<"/v1/job/", Id/binary>>, <<"POST">>, Req, State) ->
%% api_job_update(Id, Req, State);

%% Delete a specific job
%% filter_path(<<"/v1/job/", Id/binary>>, <<"DELETE">>, Req, State) ->
%% api_job_delete(Id, Req, State);

%% List jobs
%% filter_path(<<"/v1/jobs">>, <<"GET">>, Req, State) ->
%% api_jobs_list(Req, State);

%% Get a specific job by ID
%% filter_path(<<"/v1/job/", Id/binary>>, <<"GET">>, Req, State) ->
%% api_job_get(Id, Req, State);

filter_path(_, _Method, _Req, _State) ->
    {error, not_found}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_sort_json(Req, _State) ->
    case cs_tasks_job:new_from_request(Req) of 
        {ok, Job, NewReq} -> 
            case cs_sort_kahn:sort(Job) of 
                {ok, SortedJob} -> 
                    SerializedMap = cs_tasks_job:serialize_to_map(SortedJob),
                    {ok, SerializedMap, NewReq};
                {error, Error} -> {ok, #{error => Error}, Req};
                Error -> {ok, #{error => Error}, Req}
            end;
        _ -> {error, bad_request}
    end.

api_sort_bash(Req, _State) ->
    case cs_tasks_job:new_from_request(Req) of 
        {ok, Job, NewReq} -> 
            case cs_sort_kahn:sort(Job) of 
                {ok, SortedJob} -> 
                    SerializedBash = cs_tasks_job:serialize_to_bash(SortedJob),
                    {ok, #{<<"bash">> => SerializedBash}, NewReq};
                {error, Error} -> {ok, #{error => Error}, Req};
                Error -> {ok, #{error => Error}, Req}
            end;
        _ -> {error, bad_request}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Reply
reply(HttpCode, Body, CowboyReq) ->
    EncodedBody = cs_json:encode(Body),
    cowboy_req:reply(
        HttpCode,
        %% No cache
        #{
            <<"content-type">> => <<"application/json">>,
            <<"cache-control">> => <<"max-age=0, must-revalidate">>
        },
        EncodedBody,
        CowboyReq
    ).

%% @doc Reply scuccesfully
reply_ok(Body, CowboyReq) -> 
    reply(200, Body, CowboyReq).

%% @doc Reply error
reply_error({error, bad_request}, CowboyReq) ->
    reply(400, #{msg => <<"400 Bad Request">>}, CowboyReq);
reply_error({error, not_found}, CowboyReq) ->
    reply(404, #{msg => <<"404 Not Found">>}, CowboyReq);
reply_error(_, CowboyReq) ->
    reply(500, #{msg => <<"500 Internal Server Error">>}, CowboyReq).