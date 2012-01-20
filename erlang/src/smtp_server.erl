%%% 
%%% Simple SMTP server in Erlang
%%%
%%% Author: Maarten Oelering
%%%

-module(smtp_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, hostname, session}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({LSock, Hostname}) ->
  gen_server:start_link(?MODULE, [{LSock, Hostname}], []).

%%%===================================================================
%%% callbacks
%%%===================================================================

init([{LSock, Hostname}]) ->
  % force timeout and wait for connect in handle_info/2
  {ok, #state{lsock = LSock, hostname = Hostname}, 0}.

handle_call(Request, _From, State) ->
  Reply = {ok, Request},
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

%% data received on socket
handle_info({tcp, _Socket, Data}, #state{session = Session} = State) ->
  case smtp_session:data_line(Session, Data) of
    {ok, NewSession} -> 
      {noreply, State#state{session = NewSession}};
    {stop, NewSession} -> 
      {stop, normal, State#state{session = NewSession}}
  end;

%% socket closed by peer  
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
  
handle_info(timeout, #state{lsock = LSock, hostname = Hostname} = State) ->
  % wait for connection
  {ok, Socket} = gen_tcp:accept(LSock),
  {ok, Session} = smtp_session:connect(Socket, Hostname),
  % start new acceptor process
  smtpd_sup:start_child(),
  {noreply, State#state{session = Session}};
  
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{session = Session} = _State) ->
  smtp_session:disconnect(Session),
  % ?? gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% helpers
%%%===================================================================

