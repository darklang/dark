# BwdServer

This is the webserver for darklang.io, often referred to as "BWD"
(named after our old domain - builtwithdark.com).

All Dark HTTP Handlers hosted by our cloud are hosted with BWD -
our grand-users hit this server, and BWD handles the requests.

So, BWD is what handles the HTTP "handlers" of a Dark program.

It uses ASP.NET directly, instead of a web framework,
so we can tune the exact behaviour of headers and such.
