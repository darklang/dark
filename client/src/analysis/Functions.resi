@ppx.deriving(show({with_path: false})) type rec t = Types.functionsType

@ppx.deriving(show({with_path: false})) type rec props = Types.functionsProps

/* Returns the function named `name`. Returns Nothing if the function
 * can't be found - this shouldn't happen in theory but often does
 * in practice; for example, someone might delete a function and
 * then do a local undo. */
let find: (ProgramTypes.FQFnName.t, t) => option<Types.function_>

let findByStr: (string, t) => option<Types.function_>

/* For legacy reasons, we stash a global reference to the result of the latest
 * update. This was only to be used by OldExpr, but it snuck in elsewhere. DO
 * NOT USE! */
let global: unit => t

let empty: t

let asFunctions: t => list<Types.function_>

let builtins: t => list<Types.function_>

let setBuiltins: (list<RuntimeTypes.BuiltInFn.t>, props, t) => t

let setPackages: (Types.packageFns, props, t) => t

/* Update the cached function data; should be called anytime the functions
 * change. */
let update: (props, t) => t

/* For testing */
let testCalculateUnsafeUserFunctions: (props, t) => Tc.Set.String.t
