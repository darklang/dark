TODOs.md

OK - this PR is going great, and I'm nearing being comfortable merging it, but

- [x] some DB feedback...
	(all of these should involve updates to .sql migration files _and_ updates downstream, in .fs and .dark files)
	- [x] let's remove the accounts and access table - as far as I recall, nothing _really_ uses them. If there are DB references etc to accounts, let's remove that stuff
	- [x] the 'branches' table: title->name, remove 'state' and `last_active_at`. and I suppose remove `created_by` until we figure out accounts
	- [x] `backend/migrations/20251024_000003_seed_default_instance.sql` might need some work. I feel like (1) remove that script (2) the 'run-second-instance' script should be updated to insert 1 row into each DB (if nto already done) for the _other_ instance - instead of this 'migration' script
- [x] can prob remove Account.fs and related stuff
- [x] I think the BranchState model+usage can be killed
- [x] ensureMainBranch (top to bottom) can be removed - idk why that's a thing

- [ ] put TODOs in the BuiltinsPM that we should reconsider which fns should be fully public, or admin-only, etc.
- [x] remove anything referring to a patch (probably just comments - like I see a comment in a migration script)
- [x] the changes in backend/src/LibExecution/Interpreter.fs are trash revert them
- [ ] package reloading is taking like 3x as long as it used to (before this branch/PR). can you identify/diagnose why that might be? Any tips to improving that perf? Probably something in localexec...
- [ ] the CLI's prompt line used to include the 'current path' as set by 'cd' etc. but now it doesn't. can we fix that?
- [ ] I feel like we have `withExtraOps` defined in multiple places. could we reasonably consolidate? I could be wrong.
- [ ] backend/src/LibPackageManager/Stats.fs doesn't account for branches. can/should it?
- [ ] backend/src/LibParser/Canvas.fs hasn't been updated like TestModule.fs for 2 phase whatever. it should be.
- [ ] that said, the stuff in backend/src/LibParser/TestModule.fs is kinda meh - it's doing WAY too much logic stuff around op-applications. that should be handled below in LibPM or something
- [ ] I'm not even sure how the canvas is loading considering the inserts have been commented in backend/src/LocalExec/Canvas.fs ?
- [ ] in backend/tests/TestUtils/TestUtils.fs, there's no reason `let testPackageFn` needs to take the args that are now being ignored... there are probably other fns that create package things that _used_ to include a 'name' (components) and can now be tidied up further.
	- [ ] e.g. backend/tests/Tests/PT2RT.Tests.fs, `let t` on line 1625
- [ ] `(LanguageTools.WrittenTypesToProgramTypes.Context { currentFnName = Stdlib.Option.Option.None; argMap = Stdlib.Dict.empty })` is a bit wordy, maybe there should be a 'defaultContext' or something to make this tidier?
- [ ] `// CLEANUP this doesn't exist any more.` what should we do about this?
- [ ] `/ TODO: this Completion module could be extracted to its own file (utils/completion.dark)` OK I think it's time to do this that file is getting huge
- [x] in `packages/darklang/cli/packages/branch.dark` -- `clear` and `unset` and `none` -- those are all the same. They should be consolidated with 'clear' as the real name and the others simply aliases
- [x] broadly, whenever you have `let help (state: Cli.AppState) : Cli.AppState =` don't make a BUNCH of printLine usages. Rather have a [] filled with the lines and pipe it into a `Stdlib.printLines` usage - there are examples in other files
- [ ] (relevant to multiple layers) `packages/darklang/cli/packages/history.dark` this might be possible to be consolidated based on recent changes to ... actually, for now, let's remove the 'history' command and put it for later (later.md)
- [ ] packages/darklang/cli/packages/nav.dark is too long. the interactive stuff, certainly, should be extracted to some other file. I'm not sure how else to shorten things, but gosh that's long. it's worth a _lot_ of thought to tidy things up a bit. plz don't break anything tho
- [ ] packages/darklang/cli/packages/rename.dark can be safely removed with the _intent_ noted in later.md
- [ ] I'm not sure what `Display values (temporarily without pretty printing to test LocatedItem fix)` is about...
- [ ] there's an inline None for the branch when calling the builtin in packages/darklang/internal/darklang-internal-mcp-server/tools.dark -- can we pass some legit branchID in?
- [ ] migratet he comment at the bottom of packages/darklang/languageTools/lsp-server/aaaa-state.dark to 'later.md'
- [ ] I see some instances of `t.location.owner ++ "." ++ (Stdlib.String.join item.location.modules ".") ++ "." ++ item.location.name` - that should just be replaced with a call-out to something in the pretty-printer module(s) for PackageLocation
- [ ]

- [ ] I wonder if backend/src/LibParser/NameResolver.fs could be greatly tidied now that we've done a big refactor includig the 'locations' table stuff. maybe the F# equivalent too? At least, maybe there are some new helpers to extract, idk

- [ ] separately from all of this - i'd like a full builtins.md report on all the existing builtins. keep it complete but concise


For anything we remove but might need later, let's create a section around the topic in `later.md`, with all relevant details

try to create separate commits for each logical change/improvement here. this will likely take a long time.