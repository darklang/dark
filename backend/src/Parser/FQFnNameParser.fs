/// Parse and print Fully-Qualified Function Names
///
/// This is mostly used to read Darklang source strings
module Parser.FQFnNameParser

open Prelude

module PT = LibExecution.ProgramTypes

/// Standard Library Function Name
let oneWordFunctions =
  Set [ "equals"; "notEquals"; "equals_v0"; "notEquals_v0"; "emit_v1" ]

let namePat = @"^[a-z][a-z0-9_]*$"
let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
let fnnamePat = @"^([a-z][a-z0-9A-Z_]*|[-+><&|!=^%/*]{1,2})$"

let userFnNamePat = @"^.*$"

let packageFnName
  (owner : string)
  (package : string)
  (module_ : string)
  (function_ : string)
  (version : int)
  : PT.FQFnName.PackageFnName =
  assertRe "owner must match" namePat owner
  assertRe "package must match" namePat package
  if module_ <> "" then assertRe "modName name must match" modNamePat module_
  assertRe "package function name must match" fnnamePat function_
  assert_ "version can't be negative" [ "version", version ] (version >= 0)

  { owner = owner
    package = package
    module_ = module_
    function_ = function_
    version = version }

let packageFqName
  (owner : string)
  (package : string)
  (module_ : string)
  (function_ : string)
  (version : int)
  : PT.FQFnName.T =
  PT.FQFnName.Package(packageFnName owner package module_ function_ version)

let userFnName (fnName : string) : PT.FQFnName.UserFnName =
  // CLEANUP we would like to enable this, but some users in our DB have functions
  // named with weird characters, such as a url.
  assertRe "user function name must match" userFnNamePat fnName
  fnName


let userFqName (fnName : string) = PT.FQFnName.User(userFnName fnName)

let stdlibFnName
  (module_ : string)
  (function_ : string)
  (version : int)
  : PT.FQFnName.StdlibFnName =
  if module_ <> "" then assertRe "modName name must match" modNamePat module_
  assertRe "stdlib function name must match" fnnamePat function_
  assert_ "version can't be negative" [ "version", version ] (version >= 0)
  { module_ = module_; function_ = function_; version = version }

let stdlibFqName
  (module_ : string)
  (function_ : string)
  (version : int)
  : PT.FQFnName.T =
  PT.FQFnName.Stdlib(stdlibFnName module_ function_ version)

let binopFnName (op : string) : PT.FQFnName.StdlibFnName = stdlibFnName "" op 0

let binopFqName (op : string) : PT.FQFnName.T = stdlibFqName "" op 0

let parse (fnName : string) : PT.FQFnName.T =
  match fnName with
  | Regex "^([a-z][a-z0-9_]*)/([a-z][a-z0-9A-Z]*)/([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
          [ owner; package; module_; name; version ] ->
    packageFqName owner package module_ name (int version)
  | Regex "^([a-z][a-z0-9_]*)/([a-z][a-z0-9A-Z]*)/([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)$"
          [ owner; package; module_; name ] ->
    packageFqName owner package module_ name 0
  | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
          [ module_; name; version ] -> stdlibFqName module_ name (int version)
  | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
          [ module_; name; version ] -> stdlibFqName module_ name (int version)
  | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)$" [ module_; name ] ->
    stdlibFqName module_ name 0
  | Regex "^([-+><&|!=^%/*]{1,2})$" [ name ] -> stdlibFqName "" name 0
  | Regex "^([-+><&|!=^%/*]{1,2})_v(\d+)$" [ name; version ] ->
    stdlibFqName "" name (int version)
  // don't accidentally parse these as userFns
  | v when Set.contains v oneWordFunctions ->
    match v with
    | Regex "^([a-z][a-z0-9A-Z]*)_v(\d+)$" [ name; version ] ->
      stdlibFqName "" name (int version)
    | Regex "^([a-z][a-z0-9A-Z]*)$" [ name ] -> stdlibFqName "" name 0
    | _ ->
      Exception.raiseInternal
        "Bad format in one word function name"
        [ "fnName", fnName ]
  | Regex "^([a-z][a-z0-9A-Z_]*)$" [ name ] -> userFqName name
  // CLEANUP People have the most ridiculous names in userFunctions. One user had a
  // fully qualified url in there! Ridiculous. This needs a data cleanup before it
  // can be removed. Also some functions have "_v2" or similar in them.
  | Regex "^(.*)$" [ name ] -> userFqName name
  | _ -> Exception.raiseInternal "Bad format in function name" [ "fnName", fnName ]

