%%% 
%%% Simple SMTP server in Erlang
%%%
%%% Author: Maarten Oelering
%%%

-module(smtp_session).

%% API
-export([connect/2, disconnect/1, data_line/2]).

-record(session, {socket, mode, hostname}).

%%%===================================================================
%%% API
%%%===================================================================

connect(Socket, Hostname) ->
  gen_tcp:send(Socket, reply(220, lists:concat([Hostname, " ESMTP"]))),
  {ok, #session{socket = Socket, mode = command, hostname = Hostname}}.

disconnect(_Session) ->
  {ok}.
  
data_line(#session{mode = command} = Session, Data) ->
  Command = string:to_upper(string:substr(Data, 1, 4)),
  case Command of
    "HELO" -> 
      send(Session, reply(250, Session#session.hostname)),
      {ok, Session};
    "EHLO" -> 
      send(Session, reply(250, Session#session.hostname)),
      {ok, Session};
    "MAIL" -> 
      send(Session, reply(250, "OK")),
      {ok, Session};
    "RCPT" -> 
      send(Session, reply(250, "OK")),
      {ok, Session};
    "DATA" -> 
      send(Session, reply(354, "End data with <CR><LF>.<CR><LF>")),
      {ok, Session#session{mode = data}};
    "QUIT" -> 
      send(Session, reply(221, lists:concat([Session#session.hostname, " closing connection"]))),
      {stop, Session};
    _ -> 
      send(Session, reply(500, "unrecognized command")),
      {ok, Session}
  end;

data_line(#session{mode = data} = Session, Data) ->
  case Data of
    ".\r\n" ->
      send(Session, reply(250, "OK")),
      {ok, Session#session{mode = command}};
    _ ->
      {ok, Session}
  end.

%%%===================================================================
%%% Implementation
%%%===================================================================

send(#session{socket = Socket}, Reply) ->
  gen_tcp:send(Socket, Reply).

%% construct multi or singleline reply

reply(_, []) -> []; 
  
reply(Code, [Tail]) when is_list(Tail) -> 
  reply(Code, Tail);
  
reply(Code, [Head | Tail]) when is_list(Head) -> 
  [lists:concat([Code, "-", Head, "\r\n"]) | reply(Code, Tail)];

reply(Code, Text) -> 
  [lists:concat([Code, " ", Text, "\r\n"])].


