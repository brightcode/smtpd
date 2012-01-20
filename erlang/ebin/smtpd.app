{application, smtpd,
 [{description, "A simple SMTP server"},
  {vsn, "0.1.0"},
  {modules, [smtpd_app,
             smtpd_sup,
	     smtp_server, smtp_session]},
  {registered, [smtpd_sup]},
  {applications, [kernel, stdlib]},
  {mod, {smtpd_app, []}}
 ]}.
