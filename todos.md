- [x] still failing to build... (reverted bad refactoring)
- [x] is there a way to adjust vscode-extension/client/src/commands/branchCommands.ts to not compare magic strings? (added action property)
- [x] Like maybe compare something other than the label? idk if data fields or id would work. (using action field now)
- [x] the `darklang.instance.switch` command can be removed fully. (removed)
- [x] vscode-extension/client/src/fileSystemProvider.ts - fileSystemProvider was replaced with darkFileSystemProvider, they're not the same but darkFileSystemProvider is the current implementation
- [x] packages still not loading. (fixed by reverting bad refactoring)
- [x] what's going on w/ the 'type' fields in vscode-extension/client/src/types/index.ts - seems dumb and has a bunch of dead stuff. (removed PatchNode, DemoScenario, ValidationError)
- [x] could vscode-extension/package.json have some // comments to break down sections like of commands? Not a lot, just in places where there's too much clutter. (added organizing comments)
- [x] in the ops table, have we made a column that tracks whether it's been applied yet? if not, that might be a thing to do. (added applied column + updated Inserts.fs)
- [x] it feels to me as though backend/src/LibParser/TestModule.fs has too much "id stabilization" logic that should be centralized and reused somewhere else I bet we have duplication. like maybe backend/src/LocalExec/LoadPackagesFromDisk.fs. (centralized into PackageManager.stabilizeOpIds)
- [x] similarly backend/src/LocalExec/Utils.fs seems to have some overlap with 'withExtras' from LibExe/ProgramTypes.fs (removed inMemPackageManagerFromOps, using PackageManager.withExtraOps)
- [x] packages/darklang/cli/packages/location.dark is a good file, but the module its in is sort of nonsensical. move this stuff? (flattened unnecessary Create module layer)
- [ ] does packages/darklang/cli/packages/navInteractive.dark really need to be over 700 lines of code to get that functionality? maybe some major refactor would tidy it up a ton?
- [x] packages/darklang/cli/packages/remove.dark can be removed - not a command we _actually_ support at this time. migrate idea to later.md (removed and migrated)
- [x] Cli.fs fns should take a branch (if they don't already -- they might, actually) (already done - all CLI builtins take branchID as first parameter)
- [x] migrate any direct sql querying from BuiltinPM to LibPM (created LibPackageManager.Queries with getRecentOps, getRecentOpsAllBranches, getOpsSince, getNameHistory)
- [x] packages/darklang/prettyPrinter/packages.dark got deleted - maybe a module.dark would be good to replace it? i suspect some downstream stuff should actually be consolidated here. (checked - nothing uses it, deletion was intentional as part of packageop consolidation)
- [x] the centralize pretty-printer functions _should_ take in a branchId and respect it. the upstream fns that get the Location for an ID should definiltely consider branchID somehow. this will be an involved task requiring adjustments on a few layers. plan accordingly and just commit once. (added branchID to Context, threaded through typeReference/customType/fullForReference, updated LSP and CLI callers)
- [x] this block in packages/darklang/scm/sync.dark must already exist in stdlib somewhere (replaced with Stdlib.HttpClient.toString)
  match err with
                    | BadUrl details -> $"Bad URL: {details}"
                    | Timeout -> "Request timeout"
                    | BadHeader header -> $"Bad header: {header}"
                    | NetworkError -> "Network error"
                    | BadMethod -> "Bad method"
- [x] do we actually use pretty-printing of PackageOps anywhere? (yes - packageLocation used in 7 places, packageOp used in pendingOps.dark)


AI agent: ignore the following for now... still needs to be thought through more.
- path isn't respected when creating vals
- need to show sync success message (vscode extension)
- make the canvas work again (search....)
- handle unparseable .dark files - need to workshop ideas for how to represent/track these in the system