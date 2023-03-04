/// Provides toString and fromString functions for program types
///
/// We are moving to not needing parsers, but for now they are still necessary. They
/// mostly convert to a format used in the database, or in the OCaml compatible
/// representation (spoken by the client and used in the API for example)
module LibExecution.ProgramTypesParser

// CLEANUP remove this entire file once we've removed the add_ops format

// CLEANUP: almost all users of functions in this file could be replaced by having
// better types and/or propagating the type definitions end-to-end (such as to the
// client). It would likely also increase the reliability and reduce the
// maintainance costs

open Prelude
open VendoredTablecloth

module PT = ProgramTypes

/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =
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


module DType =
  let parse (str : string) : Option<PT.DType> =
    let any = PT.TVariable "a"

    match String.toLowercase str with
    | "any" -> Some any
    | "int" -> Some PT.TInt
    | "integer" -> Some PT.TInt
    | "float" -> Some PT.TFloat
    | "bool" -> Some PT.TBool
    | "boolean" -> Some PT.TBool
    | "nothing" -> Some PT.TUnit
    | "character"
    | "char" -> Some PT.TChar
    | "str" -> Some PT.TStr
    | "string" -> Some PT.TStr
    | "list" -> Some(PT.TList any)
    | "tuple" -> Some(PT.TTuple(any, any, []))
    | "obj" -> Some(PT.TDict any)
    | "block" -> Some(PT.TFn([ PT.TVariable "a" ], PT.TVariable "b"))
    | "incomplete" -> Some PT.TIncomplete
    | "error" -> Some PT.TError
    | "response" -> Some(PT.THttpResponse any)
    | "datastore" -> Some(PT.TDB any)
    | "date" -> Some PT.TDateTime
    | "password" -> Some PT.TPassword
    | "uuid" -> Some PT.TUuid
    | "option" -> Some(PT.TOption any)
    | "result" -> Some(PT.TResult(PT.TVariable "a", PT.TVariable "b"))
    | "dict" -> Some(PT.TDict any)
    | _ ->
      let parseListTyp (listTyp : string) : Option<PT.DType> =
        match String.toLowercase listTyp with
        | "str" -> Some(PT.TDbList PT.TStr)
        | "string" -> Some(PT.TDbList PT.TStr)
        | "char" -> Some(PT.TDbList PT.TChar)
        | "character" -> Some(PT.TDbList PT.TChar)
        | "int" -> Some(PT.TDbList PT.TInt)
        | "integer" -> Some(PT.TDbList PT.TInt)
        | "float" -> Some(PT.TDbList PT.TFloat)
        | "bool" -> Some(PT.TDbList PT.TBool)
        | "boolean" -> Some(PT.TDbList PT.TBool)
        | "password" -> Some(PT.TDbList PT.TPassword)
        | "uuid" -> Some(PT.TDbList PT.TUuid)
        | "dict" -> Some(PT.TDbList(PT.TDict any))
        | "date" -> Some(PT.TDbList PT.TDateTime)
        | "title" -> Some(PT.TDbList PT.TStr)
        | "url" -> Some(PT.TDbList PT.TStr)
        | _ -> None

      if String.startsWith "[" str && String.endsWith "]" str then
        str |> String.dropLeft 1 |> String.dropRight 1 |> parseListTyp
      else
        None


module Handler =
  module CronInterval =
    let toString (interval : PT.Handler.CronInterval) : string =
      match interval with
      | PT.Handler.EveryDay -> "Daily"
      | PT.Handler.EveryWeek -> "Weekly"
      | PT.Handler.EveryFortnight -> "Fortnightly"
      | PT.Handler.EveryHour -> "Every 1hr"
      | PT.Handler.Every12Hours -> "Every 12hrs"
      | PT.Handler.EveryMinute -> "Every 1min"

    let parse (modifier : string) : Option<PT.Handler.CronInterval> =
      match String.toLowercase modifier with
      | "daily" -> Some PT.Handler.EveryDay
      | "weekly" -> Some PT.Handler.EveryWeek
      | "fortnightly" -> Some PT.Handler.EveryFortnight
      | "every 1hr" -> Some PT.Handler.EveryHour
      | "every 12hrs" -> Some PT.Handler.Every12Hours
      | "every 1min" -> Some PT.Handler.EveryMinute
      | _ -> None

  module Spec =

    let toName (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP (route, _method, _ids) -> route
      | PT.Handler.Worker (name, _ids) -> name
      | PT.Handler.Cron (name, _interval, _ids) -> name
      | PT.Handler.REPL (name, _ids) -> name

    let toModifier (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP (_route, method, _ids) -> method
      | PT.Handler.Worker (_name, _ids) -> "_"
      | PT.Handler.Cron (_name, interval, _ids) ->
        interval |> Option.map CronInterval.toString |> Option.defaultValue ""
      | PT.Handler.REPL (_name, _ids) -> "_"

    let toModule (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP _ -> "HTTP_BASIC"
      | PT.Handler.Worker _ -> "WORKER" // CLEANUP the DB relies on the casing
      | PT.Handler.Cron _ -> "CRON" // CLEANUP the DB relies on the casing
      | PT.Handler.REPL _ -> "REPL"

    let isComplete (s : PT.Handler.Spec) : bool =
      match s with
      | PT.Handler.HTTP ("", _, _) -> false
      | PT.Handler.HTTP (_, "", _) -> false
      | PT.Handler.Worker ("", _) -> false
      | PT.Handler.Cron ("", _, _) -> false
      | PT.Handler.Cron (_, None, _) -> false
      | PT.Handler.REPL ("", _) -> false
      | _ -> true

    // Same as a TraceInput.EventDesc
    let toEventDesc (s : PT.Handler.Spec) : Option<HandlerDesc> =
      if isComplete s then Some(toModule s, toName s, toModifier s) else None

module Toplevel =
  let toDBTypeString (tl : PT.Toplevel.T) =
    match tl with
    | PT.Toplevel.TLDB _ -> "db"
    | PT.Toplevel.TLHandler _ -> "handler"
    | PT.Toplevel.TLFunction _ -> "user_function"
    | PT.Toplevel.TLType _ -> "user_tipe"
