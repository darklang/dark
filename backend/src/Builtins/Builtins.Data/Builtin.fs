/// The user's own data: the Darklang user database, the raw SQLite floor under it, execution
/// traces, and accounts.
///
/// The other half of the former `Builtins.Matter`; see `Builtins.Store.Builtin` for why they split.
///
/// This assembly ships FOUR platforms, for the reason `Builtins.Cli` ships four: a platform is a
/// value, and four records over four subsets of one project cost nothing at build time. Here the
/// motivation is sharper than "granting is coarser than needing". `Data` reached `Native`, and it
/// reached it because of TWO builtins out of thirty-seven. Anything wanting `DB.get` was being told
/// that granting it hands over the machine, which was true of the platform and false of the
/// function.
module Builtins.Data.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames = []


/// The Darklang user database: `Stdlib.DB`. `db-read` and `db-write`, scoped per datastore at the
/// permission check, and nothing else.
let dbPlatform : LibExecution.Platform.Platform =
  { name = "Db"
    version = 0
    description = "The user database."
    builtins = Builtin.combine [ Libs.DB.builtins () ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// Execution traces: what ran, what it was given, what it returned.
let tracesPlatform : LibExecution.Platform.Platform =
  { name = "Traces"
    version = 0
    description = "Reading and pruning execution traces."
    builtins = Builtin.combine [ Libs.Traces.builtins () ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// Accounts, by name and id. Two functions, and they are reads.
let accountsPlatform : LibExecution.Platform.Platform =
  { name = "Accounts"
    version = 0
    description = "Looking up accounts by name and id."
    builtins = Builtin.combine [ Libs.Account.builtins () ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }


/// Raw SQL. Two builtins, and the only reason anything here reaches `Native`.
///
/// `Sqlite.query` runs SQL this runtime cannot read: one `ATTACH` reaches any file on the machine,
/// so no rule could honestly scope it. Granting THIS is granting the machine, and now that is a
/// sentence about two functions instead of about thirty-seven. The effects are `dynamicEffects`
/// rather than static declarations because what a query may reach depends on the database file and
/// on whether the SQL can leave it, which the body decides; see `Libs/Sqlite.fs`.
let sqlitePlatform : LibExecution.Platform.Platform =
  { name = "Sqlite"
    version = 0
    description = "Raw SQL against a database file."
    builtins = Builtin.combine [ Libs.Sqlite.builtins () ] fnRenames
    requires = [ "Core" ]
    dynamicEffects = Libs.Sqlite.dynamicEffects
    requiresStore = true }


let platforms : List<LibExecution.Platform.Platform> =
  [ dbPlatform; tracesPlatform; accountsPlatform; sqlitePlatform ]


/// Every builtin in this assembly. The cost report and the seed exporter want the assembly rather
/// than a platform.
let builtins () : Builtins =
  Builtin.combine
    [ Libs.DB.builtins ()
      Libs.Sqlite.builtins ()
      Libs.Traces.builtins ()
      Libs.Account.builtins () ]
    fnRenames
