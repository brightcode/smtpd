%%% 
%%% Simple SMTP server in Erlang
%%%
%%% Author: Maarten Oelering
%%%

-module(smtpd_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link({LSock, Hostname}) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [{LSock, Hostname}]).

start_child() ->
  supervisor:start_child(?MODULE, []).

%%%===================================================================
%%% callbacks
%%%===================================================================
  
init([{LSock, Hostname}]) ->
  SupervisorSpec = {simple_one_for_one, 0, 1},
  ChildSpec = {smtp_server, {smtp_server, start_link, [{LSock, Hostname}]}, 
    temporary, brutal_kill, worker, [smtp_server]},
  {ok, {SupervisorSpec, [ChildSpec]}}.
  