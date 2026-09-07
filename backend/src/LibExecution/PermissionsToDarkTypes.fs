/// Typed conversion between Permissions.Policy and its Dark mirror.
module LibExecution.PermissionsToDarkTypes

open Prelude
open RuntimeTypes

module P = LibExecution.Permissions
module E = LibExecution.Effects
module Effects2DT = LibExecution.EffectsToDarkTypes
module VT = ValueType
module D = LibExecution.DvalDecoder

module Scope =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.scope ())

  let toDT
    (elementType : ValueType)
    (convert : 'a -> Dval)
    (scope : P.Scope<'a>)
    : Dval =
    match scope with
    | P.Scope.All -> DEnum(typeName (), typeName (), [ elementType ], "All", [])
    | P.Scope.Only value ->
      DEnum(typeName (), typeName (), [ elementType ], "Only", [ convert value ])

  let fromDT (convert : Dval -> 'a) (dval : Dval) : P.Scope<'a> =
    match dval with
    | DEnum(_, _, _, "All", []) -> P.Scope.All
    | DEnum(_, _, _, "Only", [ value ]) -> P.Scope.Only(convert value)
    | _ -> Exception.raiseInternal "Invalid Permissions.Scope" [ "dval", dval ]

  let stringToDT scope = toDT VT.string DString scope
  let stringFromDT dval = fromDT D.string dval
  let intToDT (scope : P.Scope<int>) =
    toDT VT.int (fun (value : int) -> Dval.int (bigint value)) scope
  let intFromDT dval = fromDT (D.int64FromInt >> int) dval

module HostRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.hostRule ())

  let toDT (rule : P.HostRule) : Dval =
    let caseName, fields =
      match rule with
      | P.HostRule.Any -> "Any", []
      | P.HostRule.Exact host -> "Exact", [ DString host ]
      | P.HostRule.SubdomainsOf host -> "SubdomainsOf", [ DString host ]
    DEnum(typeName (), typeName (), [], caseName, fields)

  let fromDT (dval : Dval) : P.HostRule =
    match dval with
    | DEnum(_, _, _, "Any", []) -> P.HostRule.Any
    | DEnum(_, _, _, "Exact", [ DString host ]) -> P.HostRule.Exact host
    | DEnum(_, _, _, "SubdomainsOf", [ DString host ]) ->
      P.HostRule.SubdomainsOf host
    | _ -> Exception.raiseInternal "Invalid Permissions.HostRule" [ "dval", dval ]

module HttpRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.httpRule ())

  let toDT (rule : P.HttpRule) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map
        [ "method", Scope.stringToDT rule.method
          "scheme", Scope.stringToDT rule.scheme
          "host", HostRule.toDT rule.host
          "port", Scope.intToDT rule.port
          "pathPrefix", Scope.stringToDT rule.pathPrefix
          "query", Scope.stringToDT rule.query ]
    )

  let fromDT (dval : Dval) : P.HttpRule =
    match dval with
    | DRecord(_, _, _, fields) ->
      { method = Scope.stringFromDT (D.field "method" fields)
        scheme = Scope.stringFromDT (D.field "scheme" fields)
        host = HostRule.fromDT (D.field "host" fields)
        port = Scope.intFromDT (D.field "port" fields)
        pathPrefix = Scope.stringFromDT (D.field "pathPrefix" fields)
        query = Scope.stringFromDT (D.field "query" fields) }
    | _ -> Exception.raiseInternal "Invalid Permissions.HttpRule" [ "dval", dval ]

module ProcessRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.processRule ())

  let toDT (rule : P.ProcessRule) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map
        [ "executable", Scope.stringToDT rule.executable
          "args",
          Scope.toDT
            (VT.list VT.string)
            (fun (args : List<string>) -> DList(VT.string, List.map DString args))
            rule.args ]
    )

  let fromDT (dval : Dval) : P.ProcessRule =
    match dval with
    | DRecord(_, _, _, fields) ->
      { executable = Scope.stringFromDT (D.field "executable" fields)
        args = Scope.fromDT (D.list D.string) (D.field "args" fields) }
    | _ -> Exception.raiseInternal "Invalid Permissions.ProcessRule" [ "dval", dval ]

module Rule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.rule ())
  let knownType () = KTCustomType(typeName (), [])

  let private make caseName fields =
    DEnum(typeName (), typeName (), [], caseName, fields)

  let toDT (rule : P.Rule) : Dval =
    match rule with
    | P.Rule.All -> make "All" []
    | P.Rule.Effect effect -> make "Effect" [ Effects2DT.Effect.toDT effect ]
    | P.Rule.Http http -> make "Http" [ HttpRule.toDT http ]
    | P.Rule.HttpServer ports -> make "HttpServer" [ Scope.intToDT ports ]
    | P.Rule.File(P.AccessKind.Read, roots) ->
      make "FileRead" [ Scope.stringToDT roots ]
    | P.Rule.File(P.AccessKind.Write, roots) ->
      make "FileWrite" [ Scope.stringToDT roots ]
    | P.Rule.Env(P.AccessKind.Read, names) ->
      make "EnvRead" [ Scope.stringToDT names ]
    | P.Rule.Env(P.AccessKind.Write, names) ->
      make "EnvWrite" [ Scope.stringToDT names ]
    | P.Rule.Db(P.AccessKind.Read, names) -> make "DbRead" [ Scope.stringToDT names ]
    | P.Rule.Db(P.AccessKind.Write, names) ->
      make "DbWrite" [ Scope.stringToDT names ]
    | P.Rule.Process processRule -> make "Process" [ ProcessRule.toDT processRule ]

  // Rule roots written by tooling arrive as user-typed strings. Requests are
  // normalized (`~`/relative → absolute) before checking, so a rule kept
  // verbatim would silently never match. Normalize at the same boundary.
  let private normalizeRoot (root : string) : string =
    let expanded =
      if root = "~" then
        System.Environment.GetFolderPath(
          System.Environment.SpecialFolder.UserProfile
        )
      elif root.StartsWith "~/" then
        System.IO.Path.Combine(
          System.Environment.GetFolderPath(
            System.Environment.SpecialFolder.UserProfile
          ),
          root.Substring 2
        )
      else
        root
    try
      System.IO.Path.GetFullPath expanded
    with _ ->
      root

  let private normalizedRoot (scope : P.Scope<string>) : P.Scope<string> =
    match scope with
    | P.Scope.All -> P.Scope.All
    | P.Scope.Only root -> P.Scope.Only(normalizeRoot root)

  let fromDT (dval : Dval) : P.Rule =
    match dval with
    | DEnum(_, _, _, "All", []) -> P.Rule.All
    | DEnum(_, _, _, "Effect", [ effect ]) ->
      P.Rule.Effect(Effects2DT.Effect.fromDT effect)
    | DEnum(_, _, _, "Http", [ rule ]) -> P.Rule.Http(HttpRule.fromDT rule)
    | DEnum(_, _, _, "HttpServer", [ ports ]) ->
      P.Rule.HttpServer(Scope.intFromDT ports)
    | DEnum(_, _, _, "FileRead", [ roots ]) ->
      P.Rule.File(P.AccessKind.Read, Scope.stringFromDT roots |> normalizedRoot)
    | DEnum(_, _, _, "FileWrite", [ roots ]) ->
      P.Rule.File(P.AccessKind.Write, Scope.stringFromDT roots |> normalizedRoot)
    | DEnum(_, _, _, "EnvRead", [ names ]) ->
      P.Rule.Env(P.AccessKind.Read, Scope.stringFromDT names)
    | DEnum(_, _, _, "EnvWrite", [ names ]) ->
      P.Rule.Env(P.AccessKind.Write, Scope.stringFromDT names)
    | DEnum(_, _, _, "DbRead", [ names ]) ->
      P.Rule.Db(P.AccessKind.Read, Scope.stringFromDT names)
    | DEnum(_, _, _, "DbWrite", [ names ]) ->
      P.Rule.Db(P.AccessKind.Write, Scope.stringFromDT names)
    | DEnum(_, _, _, "Process", [ rule ]) -> P.Rule.Process(ProcessRule.fromDT rule)
    | _ -> Exception.raiseInternal "Invalid Permissions.Rule" [ "dval", dval ]

module Policy =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.policy ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT (policy : P.Policy) : Dval =
    let allow, deny = P.Policy.rules policy
    DRecord(
      typeName (),
      typeName (),
      [],
      Map
        [ "allow", DList(VT.known (Rule.knownType ()), List.map Rule.toDT allow)
          "deny", DList(VT.known (Rule.knownType ()), List.map Rule.toDT deny) ]
    )

  let fromDT (dval : Dval) : P.Policy =
    match dval with
    | DRecord(_, _, _, fields) ->
      P.Policy.create
        (D.list Rule.fromDT (D.field "allow" fields))
        (D.list Rule.fromDT (D.field "deny" fields))
    | _ -> Exception.raiseInternal "Invalid Permissions.Policy" [ "dval", dval ]

module PinFailure =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.pinFailure ())
  let knownType () = KTCustomType(typeName (), [])

  let private make caseName fields =
    DEnum(typeName (), typeName (), [], caseName, fields)

  let contractChanged (differences : List<string>) : Dval =
    make "ContractChanged" [ DList(VT.string, List.map DString differences) ]

  let refused (message : string) : Dval = make "Refused" [ DString message ]
