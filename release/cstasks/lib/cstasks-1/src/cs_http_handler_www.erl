%% Static file cowboy HTTP handler
%%
%% This handler serve files inside www directory - the frontend Single Page Application
%% written in JavaScript and used to test our solution API.
%%
%% I know cowboy comes with a ready to use handler for serving static files,
%% but again it was fun creating a static file handler that is able to cache
%% the static content into an ETS, serve any IMT known to man, pardon me
%% apache/nginx (the cs_http_imt was build studing Apache/Nginx IMT config files) and
%% a paranoic validation of the request using recursive functions and binaries.
%% Because I must say it's a hobby of mine NOT to use regex :)
-module(cs_http_handler_www).

-include("cstasks.hrl").

-export([init/2]).
-export([terminate/3]).

-define(CS_FIREWALL_SCORE_MAX, 100).

%% @doc Init callback
init(Req, State) ->
    case filter(Req, State) of
        {ok, Req1} ->
            #{
                m_path := Path,
                m_path_extension := Extension,
                m_path_last_char := LastChar
            } = Req1,
            ?CS_LOG_DEBUG(#{
                m => http_www_after_filter,
                req => Req1
            }),
            case resolve_path(Path, Extension, LastChar) of
                {ok, FileBinary, ResolvedExtension} ->
                    SuccessHeaders = get_headers_for_extension(ResolvedExtension),
                    {ok, cowboy_req:reply(200, SuccessHeaders, FileBinary, Req1), State};
                {error, _} ->
                    {ok, reply_not_found(Req1), State}
            end;
        _ ->
            %% 404 - Better not to give any clue to the caller
            {ok, reply_not_found(Req), State}
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
    filter_method(Method, Req, State).

%% @doc Filter method.
%% For static content we only allow GET, HEAD and OPTIONS.
filter_method(Method, Req, _State) when
    Method =:= <<"GET">>;
    Method =:= <<"HEAD">>;
    Method =:= <<"OPTIONS">>
->
    Path = cowboy_req:path(Req),
    case cs_validate:safe_http_www_path(Path) of
        {ok, MPath, MExtension, MPathLastChar} ->
            filter_path(MPath, MExtension, MPathLastChar, Req);
        _Error ->
            {error, not_found}
    end;
filter_method(_, Req, _State) ->
    firewall_ban(Req, error_invalid_method, ?CS_FIREWALL_SCORE_MAX).

%% @doc Filter path
filter_path(_Path, Extension, _LastChar, Req) when
    Extension =:= <<"php">>;
    Extension =:= <<"aspx">>;
    Extension =:= <<"sql">>
->
    firewall_ban(Req, error_invalid_extension, ?CS_FIREWALL_SCORE_MAX);
filter_path(Path, Extension, LastChar, Req) ->
    Req1 = maps:put(m_path, Path, Req),
    Req2 = maps:put(m_path_extension, Extension, Req1),
    Req3 = maps:put(m_path_last_char, LastChar, Req2),
    {ok, Req3}.

%% @doc We can improve this method by sending data to some sort of behaiviour based firewall
%% microservice or application that will ban the remote agent IP depending on the score (for example).
%% We can in theory protect the entire cluster not only a specific server with the data we gather.
firewall_ban(Req, Reason, Score) ->
    ?CS_LOG_WARNING(#{m => firewall_http_ban, req => Req, reason => Reason, score => Score}),
    {error, drop}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% resolve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Root and empty paths resolve to index.html
resolve_path(Path, _Extension, _LastChar) when
    Path =:= <<>>;
    Path =:= <<$/>>
->
    resolve_resource(<<"index.html">>, <<"html">>);
%% Remove any path separator from the begining (remember previous filter checked for
%% double path separators so we don't need to bother about this scenario)
resolve_path(<<$/, Path/binary>>, Extension, LastChar) ->
    resolve_path(Path, Extension, LastChar);
%% Any attempt to list directories resolve to index.html
resolve_path(Path, _Extension, $/) ->
    resolve_resource(<<Path/binary, "index.html">>, <<"html">>);
%% Path with extensions are valid files
resolve_path(Path, Extension, _LastChar) when erlang:is_binary(Extension) ->
    resolve_resource(<<Path/binary>>, Extension);
%% Any path without extension resolve to .html
resolve_path(Path, undefined, LastChar) ->
    resolve_path(<<Path/binary, ".html">>, <<"html">>, LastChar);
%% Anything else is invalid
resolve_path(_Path, _Extension, _LastChar) ->
    {error, not_found}.

%% We should not reach this point with a resource starting with a $/ character
%% However make sure we catch this
resolve_resource(Path = <<$/, _/binary>>, Extension) ->
    ?CS_LOG_ERROR(#{m => path_starting_with_separator, path => Path, extension => Extension}),
    {error, not_found};
resolve_resource(Resource, Extension) ->
    ?CS_LOG_DEBUG(#{m => resolve_resource, path => Resource, extension => Extension}),
    WWWDirectory = cs_utils:directory_www(),
    ?CS_LOG_DEBUG(#{directory => WWWDirectory}),
    Filename = filename:join([WWWDirectory, Resource]),
    case filelib:is_regular(Filename) of
        true ->
            read_file(Filename, Extension);
        false ->
            ?CS_LOG_DEBUG(#{
                m => resource_not_found,
                filename => Filename
            }),
            {error, not_found}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Read file from filesystem or from ETS cache if serving from cache is enabled
read_file(File, Extension) ->
    CacheEnabled = application:get_env(?CS_APP_NAME, www_enable_cache, false),
    read_file(CacheEnabled, File, Extension).

%% Read file from filesystem or from ETS cache if the file is already in the ETS cache
read_file(true, File, Extension) ->
    Record = ets:lookup(?CS_ETS_TABLE_CACHE_WWW, File),
    case Record of
        [{File, {EtsBinary}}] ->
            {ok, EtsBinary, Extension};
        _ ->
            Result = file:read_file(File),
            case Result of
                {ok, ReadBinary} ->
                    true = ets:insert(?CS_ETS_TABLE_CACHE_WWW, {File, {ReadBinary}}),
                    {ok, ReadBinary, Extension};
                Error ->
                    ?CS_LOG_DEBUG(#{
                        m => error_read_www_file,
                        file => File,
                        error => Error
                    }),
                    Error
            end
    end;
%% Read file from filesystem when cache is disabled
read_file(false, File, Extension) ->
    Result = file:read_file(File),
    case Result of
        {ok, ReadBinary} ->
            {ok, ReadBinary, Extension};
        Error ->
            ?CS_LOG_DEBUG(#{
                m => error_read_www_file,
                file => File,
                error => Error
            }),
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reply_not_found(CowboyReq) ->
    cowboy_req:reply(
        404,
        #{
            <<"content-type">> => <<"text/html">>,
            <<"cache-control">> => <<"max-age=0, must-revalidate">>
        },
        <<"404 Not Found">>,
        CowboyReq
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_headers_for_extension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_headers_for_extension(Extension) ->
    FileIMT = cs_http_imt:extension_to_type(Extension, <<"application/octet-stream">>),
    #{
        <<"content-type">> => FileIMT,
        <<"cache-control">> => <<"max-age=0, must-revalidate">>
        %% Caching static assets with "cache busting"?
        %% https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
        %% 365 days
        %% <<"cache-control">> => <<"max-age=31536000, immutable">>
    }.
