-module(cs_sup).
-author("Madalin Grigore-Enescu").

-behaviour(supervisor).

-include("cstasks.hrl").

%% supervisor exports
-export([start_link/0]).
-export([init/1]).
-export([upgrade/0]).

%% @doc Creates a supervisor process as part of a supervision tree.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Returns supervisor flags and child specifications.
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    % ChildSpecs = [#{id => cs_sqlite,
    %                 start => {cs_sqlite, start_link, []},
    %                 restart => permanent,
    %                 shutdown => 5000,
    %                 type => worker,
    %                 modules => [cs_sqlite]}],
    {ok, {SupFlags, []}}.

%% @doc Handle the upgrade process.
upgrade() -> ok.
