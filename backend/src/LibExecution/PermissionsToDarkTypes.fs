/// Typed conversion between Permissions.Policy and its Dark mirror.
module LibExecution.PermissionsToDarkTypes

open Prelude
open RuntimeTypes

module P = LibExecution.Permissions
module E = LibExecution.Effects
module Effects2DT = LibExecution.EffectsToDarkTypes
module VT = ValueType
module D = LibExecution.DvalDecoder
module Dval = LibExecution.Dval

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
    toDT VT.int64 (fun (value : int) -> Dval.dint64 (int64 value)) scope
  let intFromDT dval = fromDT D.int dval

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

module HttpPath =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.httpPath ())

  let toDT (path : P.HttpPath) : Dval =
    let name, fields =
      match path with
      | P.HttpPath.All -> "All", []
      | P.HttpPath.Exact path -> "Exact", [ DString path ]
      | P.HttpPath.Subtree path -> "Subtree", [ DString path ]
    DEnum(typeName (), typeName (), [], name, fields)

  let fromDT (value : Dval) : P.HttpPath =
    match value with
    | DEnum(_, _, _, "All", []) -> P.HttpPath.All
    | DEnum(_, _, _, "Exact", [ DString path ]) -> P.HttpPath.Exact path
    | DEnum(_, _, _, "Subtree", [ DString path ]) -> P.HttpPath.Subtree path
    | _ -> Exception.raiseInternal "Invalid Permissions.HttpPath" [ "value", value ]

module HttpRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.httpRule ())

  let toDT (rule : P.HttpRule) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      RecordFields.ofList
        [ "method", Scope.stringToDT rule.method
          "scheme", Scope.stringToDT rule.scheme
          "host", HostRule.toDT rule.host
          "port", Scope.intToDT rule.port
          "path", HttpPath.toDT rule.path
          "query", Scope.stringToDT rule.query ]
    )

  let fromDT (dval : Dval) : P.HttpRule =
    match dval with
    | DRecord(_, _, _, fields) ->
      { method = Scope.stringFromDT (D.field "method" fields)
        scheme = Scope.stringFromDT (D.field "scheme" fields)
        host = HostRule.fromDT (D.field "host" fields)
        port = Scope.intFromDT (D.field "port" fields)
        path = HttpPath.fromDT (D.field "path" fields)
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
      RecordFields.ofList
        [ "executable", Scope.stringToDT rule.executable
          "args",
          Scope.toDT
            (VT.list VT.string)
            (fun (args : List<string>) ->
              DList(VT.string, Items(List.map DString args)))
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
    | P.Rule.HttpServer(addresses, ports) ->
      make "HttpServer" [ Scope.stringToDT addresses; Scope.intToDT ports ]
    | P.Rule.File(P.AccessKind.Read, roots) ->
      make "FileRead" [ Scope.stringToDT roots ]
    | P.Rule.File(P.AccessKind.Write, roots) ->
      make "FileWrite" [ Scope.stringToDT roots ]
    | P.Rule.Env(P.AccessKind.Read, names) ->
      make "EnvRead" [ Scope.stringToDT names ]
    | P.Rule.Env(P.AccessKind.Write, names) ->
      make "EnvWrite" [ Scope.stringToDT names ]
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
    | DEnum(_, _, _, "HttpServer", [ addresses; ports ]) ->
      P.Rule.HttpServer(Scope.stringFromDT addresses, Scope.intFromDT ports)
    | DEnum(_, _, _, "FileRead", [ roots ]) ->
      P.Rule.File(P.AccessKind.Read, Scope.stringFromDT roots |> normalizedRoot)
    | DEnum(_, _, _, "FileWrite", [ roots ]) ->
      P.Rule.File(P.AccessKind.Write, Scope.stringFromDT roots |> normalizedRoot)
    | DEnum(_, _, _, "EnvRead", [ names ]) ->
      P.Rule.Env(P.AccessKind.Read, Scope.stringFromDT names)
    | DEnum(_, _, _, "EnvWrite", [ names ]) ->
      P.Rule.Env(P.AccessKind.Write, Scope.stringFromDT names)
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
      RecordFields.ofList
        [ "allow",
          DList(VT.known (Rule.knownType ()), Items(List.map Rule.toDT allow))
          "deny", DList(VT.known (Rule.knownType ()), Items(List.map Rule.toDT deny)) ]
    )

  let fromDT (dval : Dval) : P.Policy =
    match dval with
    | DRecord(_, _, _, fields) ->
      P.Policy.create
        (D.list Rule.fromDT (D.field "allow" fields))
        (D.list Rule.fromDT (D.field "deny" fields))
    | _ -> Exception.raiseInternal "Invalid Permissions.Policy" [ "dval", dval ]

module ApprovalFailure =
  let typeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Permissions.approvalFailure ()
    )
  let knownType () = KTCustomType(typeName (), [])

  let private make caseName fields =
    DEnum(typeName (), typeName (), [], caseName, fields)

  let contractChanged (differences : List<string>) : Dval =
    make "ContractChanged" [ DList(VT.string, Items(List.map DString differences)) ]

  let refused (message : string) : Dval = make "Refused" [ DString message ]

/// An instance-policy change: replace it whole, or add and remove rules.
module PolicyEdit =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.policyEdit ())

  type T =
    | Replace of P.Policy
    | Change of allow : List<P.Rule> * deny : List<P.Rule> * remove : List<P.Rule>

  let fromDT (dval : Dval) : T =
    match dval with
    | DEnum(_, _, _, "Replace", [ policy ]) -> Replace(Policy.fromDT policy)
    | DEnum(_, _, _, "Change", [ allow; deny; remove ]) ->
      let rules = D.list Rule.fromDT
      Change(rules allow, rules deny, rules remove)
    | _ -> Exception.raiseInternal "Invalid Permissions.PolicyEdit" [ "dval", dval ]

/// Which approval record a withdrawal drops: the version approved for a name,
/// or the approval of a hash.
module ApprovalTarget =
  let typeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Permissions.approvalTarget ()
    )

  type T =
    | Name of string
    | Hash of string

  let fromDT (dval : Dval) : T =
    match dval with
    | DEnum(_, _, _, "Name", [ DString name ]) -> Name name
    | DEnum(_, _, _, "Hash", [ DString hash ]) -> Hash hash
    | _ ->
      Exception.raiseInternal "Invalid Permissions.ApprovalTarget" [ "dval", dval ]

/// One approval as an account sees it: a hash, one name approved at it (if
/// any), its stored policy, and the rules written when it was approved as a root.
module Approval =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Permissions.approval ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT
    (hash : string)
    (name : Option<string>)
    (policy : Option<P.Policy>)
    (explicitRules : Option<P.Policy>)
    : Dval =
    let policyOption (p : Option<P.Policy>) : Dval =
      Dval.option (Policy.knownType ()) (Option.map Policy.toDT p)
    DRecord(
      typeName (),
      typeName (),
      [],
      RecordFields.ofList
        [ "hash", DString hash
          "name", Dval.option KTString (Option.map DString name)
          "policy", policyOption policy
          "explicitRules", policyOption explicitRules ]
    )
