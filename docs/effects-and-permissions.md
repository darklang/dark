This document describes how the effects and permissions systems work today.

**Why we need this**

We need to understand what code might do and control what it can access when it runs. That matters whether we write the code ourselves, use a library, or ask an AI agent to write it.

We should be able to see which files it might read or change, where it might send requests, and which programs it might start. Then we can decide what to allow. When Dark cannot work something out, it tells you.

You can 

- set default rules for your Dark installation
- save an approval for the current version of a function or script
- allow access for just one run.

You control these approvals through the CLI. Code running with restricted permissions cannot approve its own requests.

### Effects: what the code might do

An effect describes something code might do, such as reading a file or making an HTTP request. 
If your function calls another function that reads a file, your function has a FileRead effect too.

These are the effects Dark tracks:

- Http: send requests, such as fetching GitHub issues.
- HttpServer: run a web server, such as a local site on localhost:9099.
- FileRead: read files or look inside folders.
- FileWrite: create, append to, overwrite or delete files.
- EnvRead: read environment variables.
- EnvWrite: set or change environment variables.
- Process: start external programs and communicate with them.
- TraceRead: read recordings of earlier runs.
- TraceWrite: write, change or delete those recordings.
- Concurrency: start Dark background work with Stdlib.Exec.spawn.
- Native: use broad system access, such as stopping a process by its ID.
- Clock: read the time or use timers.
- Random: generate random values.
- Stdin: read the input supplied to the run.
- Stdout: write output.
- PackageRead: read code and information from the local package store.
- PackageWrite: change the local package store.
- Runtime: refresh Dark's view of stored code and watch for edits.

Dark allows Clock and Random by default, but still tracks them as effects because the same inputs can produce different results. e.g. a deadline check can pass now and fail later, and generating a password should give a fresh result each time. Dark needs to account for this before reusing a saved result or a previous test pass.

The effect list shows what code might do, not what it did on a particular run. For example, FileRead means the code may read a file; it still needs permission before doing so. 

#### How Dark finds effects

We do static analysis over the function’s AST, following calls and combining their effects without running the code. Each builtin declares its effects, giving the analysis a starting point.

The analysis includes possible branches, callbacks, and the functions behind operators and trait methods when it can identify them. It also handles functions that call themselves or each other without getting stuck.

When a call cannot be resolved, Dark shows the effects it knows about and marks the result incomplete.

The effect list can include work that does not happen during a particular run. For example, if your function returns another function that reads a file, Dark may include FileRead even if the returned function is never called. The same applies to streams whose values are never read.
This is a limitation of the current analysis.

### Checking a function's effects

You can inspect a function without running it:

`dark effects fn-name`

The command shows possible effects, the calls behind them, and whether the analysis is complete

### Permissions: what the code is allowed to do

A permission controls what code can do and what it can access. An approval is your decision to give it that access.

A new Dark installation allows these without asking:

- Reading terminal input and printing output.
- Reading the time, using timers and generating random values.
- Reading the local package store.
- Reading and writing traces.
- Starting Dark background work with Stdlib.Exec.spawn. That work keeps the restrictions of the code that started it.

You can remove or deny any of these default permissions.

Access to files, HTTP, servers, environment variables and external programs needs
an additional permission. So do package writes, Runtime operations and Native operations.

You can change permissions through the CLI, but code you run with `run` or `eval` has to follow those permissions it can’t give itself more access.

#### Where access comes from

- Access comes from instance rules, saved approvals or permissions for one run.
- Instance rules set the default access for code you run in Dark:
    - Allow: Give access without asking, unless a deny rule or run restriction blocks it.
    - Deny: Block access, even when another rule or approval allows it
    - No matching rule: Dark checks whether you’ve already approved the access for this code or this run. If not, it asks you. If it cannot ask, the run stops with a clear error
- You can save access for a specific function. For example, you can approve the requests Me.GitHub.fetch needs without allowing HTTP for every function in your installation
- The approval records the function’s full name and current code hash. If the function or its dependencies change, the old approval no longer applies
- You can also allow access for just one run. For example, allow an HTTP request
this time without saving an approval for future runs.
- Temporary access and run restrictions do not change your saved approvals.

#### What happens when you run code

You can write, edit and typecheck code normally. When you use run in an interactive terminal, Dark can ask about missing permissions. Other ways of executing code, such as eval and server handlers, report a denial instead of showing this prompt.

Before starting, run checks simple requests it can work out from the code and asks about missing access together. It follows calls that will happen and simple conditions it can resolve from values written in the code.

This check is limited. Dark does not execute code to discover what it needs. Requests that depend on calculated paths, unknown conditions, callbacks or work done later are checked while running, just before the operation happens.

For example, FileRead tells you that code may read a file. It does not tell you which file. If the path becomes known later, Dark checks it before opening it.

Suppose Me.Reports.report wants to read /reports/january.csv and does not already have access. Dark offers three choices:

- Allow this run: read the file this time.
- Remember: also allow future runs of this same function and code version to read the file.
- Cancel: do not allow the read or save a new approval from this prompt.

If permission settings change while you are reviewing an approval, Dark refuses to save it and asks you to review again

If you approve a request while the code is running, Dark continues from where it paused. It does not restart the script or repeat earlier work.

If you cancel before execution begins, the function does not start. If you cancel later, Dark stops the run and blocks its work from continuing to use that access. Changes already made, such as a file write, are not undone.

If Dark cannot ask for approval, the run fails with an error explaining the missing access. A denied operation in a server handler fails that request; the server itself stays running.

#### What happens when approved code calls other code

An approval covers the reviewed function and the code it depends on. Its helpers can use that access while doing work for it.

Running a helper on its own is a separate decision. It does not inherit the approval of a function that once called it. Likewise, a helper's own saved approval cannot give an unapproved caller extra access.

Callbacks and streams keep the restrictions they had when they were created. They are also limited by the code that later calls or reads them.

A trait implementation picked by the caller, through the type of a value it passes in, works like a callback: it runs with the caller's access, not the approval's.

If you save a value that refers to a function, Dark remembers the function name it resolved to. It does not save permission to use it. Loading that value later cannot restore temporary access from an earlier run.

#### Choosing how much access to allow

**Files**

Allow access to one file, or a folder and its contents.

- /reports/january.csv covers only that file.
- /reports includes its subfolders, but not /reports-private.
- Read access allows no changes. Write access allows creating, appending, overwriting and deleting. There is no separate delete permission.
- Dark checks where folder links actually lead, so a link inside `/reports` cannot grant access to `/private`. Linux and macOS also guard against links being swapped during the operation. Windows does not yet have that protection.

**Environment variables**

Choose which variables code can read or change.

Reading APP_MODE does not allow changing it or reading other variables. Denying access to any variable also blocks listing all variables, so code cannot bypass the deny by requesting everything.

**HTTP**

Choose the host, method, protocol, port and path.

- An exact host such as `example.com` excludes subdomains. `*.example.com` includes the parent domain and all its subdomains.
- Method, protocol and port are separate limits: `GET` does not allow `POST`, `HTTPS` does not allow `HTTP`, and one port does not allow another.
- `/orders` allows only that path. `/orders/*` also allows paths below it, but not `/orders-old`. Leaving out the path allows every path on that host.
- Query parameters can vary unless the rule includes an exact query.
- Redirects need permission for the new URL.
- Code cannot override the request’s Host header; it must match the URL Dark checked
- Local and private network access also needs the appropriate HTTP settings.

**Servers**

Choose the address and port where a server can listen.

- `http-server 127.0.0.1 9099` allows local connections on port 9099.
- `http-server * 9099` allows any listening address on that port.
- `localhost` means 127.0.0.1. Other address names are not looked up.
- `serve` defaults to 127.0.0.1. Use `--bind 0.0.0.0` and allow that address to listen on all IPv4 interfaces.

Other ports need their own permission. IPv6 is not supported yet, and Windows refuses address-specific listening because its server backend cannot enforce that limit.

**External programs**

Choose an executable by its full path and, optionally, limit its arguments.

- `process /usr/bin/python3` allows any arguments.
- `process /usr/bin/python3 report.py` allows only that argument list.
- `process-exact /usr/bin/python3` allows no arguments.

These rules control starting the program. They do not restrict what it does afterward

**Traces**

Trace permissions cover all recordings in the local store:

- `trace-read` allows reading them.
- `trace-write` allows writing, changing or deleting them.

You cannot limit access to one recording or report.

#### Saved approvals and ongoing runs

- Choosing Remember adds to the saved access for the same code version. The approve and update commands can replace those rules.
- Running a function uses its current code on the selected branch. An approval never makes it run an older version.
- Scripts appear in the approval list as `script:/absolute/path.`
- Runs already in progress keep their starting code and generally keep their starting permissions.
- Removing an approval affects new runs. Interactive runs also check current
instance deny rules, so adding a deny can block their next operation.
- traces resume and traces fork check permissions again without restoring temporary access from the original run.

#### Managing permissions

To see your rules and approvals:

- permissions list shows instance allow and deny rules.
- permissions approved lists saved approvals by name and version.
- permissions show Me.reports.report shows that function’s effects and saved approvals. Actual access still depends on instance rules, run restrictions and the resource being used.

To change them:

- permissions allow, deny and remove edit instance rules.
- permissions set replaces all instance rules.
- permissions clear removes instance rules but leaves saved approvals in place.
- permissions approve saves access for a named function.
- permissions update reviews its current version again.
- permissions unapprove removes its saved approval.
- permissions revoke-package <hash> removes every named approval for that version.

For example:

- dark permissions allow file read /reports
- dark permissions deny file read /private
- dark permissions approve Me.reports.report'file read /reports'
- dark run --allow'file read  /reports' report.dark
- dark permissions unapprove Me.reports.report

Without explicit rules, approve uses the effects Dark found. This can give broad access, such as reading files in general. Supply rules when you want to limit access to particular files or hosts.

if analysis is incomplete, you can supply rules yourself or confirm an approval that gives no extra access. Dark does not guess which permissions the unknown effects need.

Removing an approval does not remove access allowed by other rules. Use a deny rule to block access regardless of approvals.

#### Starting with a profile

Profiles provide ready-made instance rules:

- default restores the installation defaults.
- read-only allows reading a chosen directory and making HTTPS GET requests on port 443, along with basic access such as terminal input and output.
- local-dev also allows writing in that directory, changing packages, picking up code edits and serving on localhost.

Both read-only and local-dev allow HTTPS GET requests to any host.

**Preview a profile before applying it:**

- dark permissions profile local-dev /home/me/project
- dark permissions profile local-dev /home/me/project --yes

 Applying a profile replaces the whole instance policy, including existing deny rules.

#### Where permissions are saved

Dark saves instance rules and named approvals in a separate binary file:
  ~/.darklang/policy/policies.bin

Alongside the code version and access rules, each approval records whether you supplied the rules or Dark inferred them. It also records the analysis version used. Changes to Dark’s analysis or builtin effect declarations can require another review.
Dark saves the whole change or keeps the previous settings. The file holds current decisions, without a history of approvals and refusals.
Missing, damaged or unsupported policy data grants no access. Only a new installation gets the default rules automatically. Older policy and stored-code formats are not automatically converted.

#### Where Dark’s protection ends

- Native access covers operations Dark cannot limit to a particular resource, such as arbitrary SQL and low-level process controls. Reading permission settings currently requires Native access too.
- Containing external programs and broad Native operations would require an operating system sandbox. Dark’s --sandbox flag blocks permission-controlled operations, but does not provide that isolation.
- An agent with an unrestricted shell can change Dark’s settings just as you can. Dark’s permission checks do not restrict an agent that already controls the shell.

#### What could improve later

- More precise effects for callbacks, returned functions and streams
- More permission checks before execution.
- Access limited to individual traces.
- Append-only or create-only file permissions.
- Rules for a whole package.
- OS isolation for external programs and Native operations.
- Stronger Windows file and server protections.
- A history of permission decisions.
- Controls on where private data can be sent. Today, code allowed to read a private file and contact a website can send that file’s contents there.
