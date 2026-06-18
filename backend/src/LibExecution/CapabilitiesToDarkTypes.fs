/// C2DT for the capability model — converts F# `LibExecution.Capabilities` values to/from their Dark
/// mirror (`Darklang.LanguageTools.Capabilities`). This is the STRUCTURED F#/Dark boundary: the caps
/// builtins hand the grant across as this value, and ALL spec-string parsing/rendering lives in Dark.
/// Mirrors the style of `ProgramTypesToDarkTypes`.
module LibExecution.CapabilitiesToDarkTypes

open Prelude
open RuntimeTypes

module Cap = LibExecution.Capabilities
module VT = ValueType
module D = LibExecution.DvalDecoder


module Scope =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.scope ())

  /// Generic over the element's value-type + converter, so it serves `Scope<String>` and `Scope<Int>`.
  let toDT (elemVT : ValueType) (elemToDT : 'a -> Dval) (s : Cap.Scope<'a>) : Dval =
    let typeArgs = [ elemVT ]
    match s with
    | Cap.Any -> DEnum(typeName (), typeName (), typeArgs, "Any", [])
    | Cap.Only xs ->
      DEnum(
        typeName (),
        typeName (),
        typeArgs,
        "Only",
        [ DList(elemVT, xs |> Set.toList |> List.map elemToDT) ]
      )

  let fromDT (elemFromDT : Dval -> 'a) (d : Dval) : Cap.Scope<'a> =
    match d with
    | DEnum(_, _, _, "Any", []) -> Cap.Any
    | DEnum(_, _, _, "Only", [ items ]) ->
      Cap.Only(D.list elemFromDT items |> Set.ofList)
    | _ -> Exception.raiseInternal "Invalid Capabilities.Scope" [ "dval", d ]

  // the two concrete element kinds the model uses
  let strToDT (s : Cap.Scope<string>) : Dval = toDT VT.string DString s
  let strFromDT (d : Dval) : Cap.Scope<string> = fromDT D.string d
  let portToDT (s : Cap.Scope<int64>) : Dval =
    toDT VT.int (fun n -> DInt(DarkInt.ofBigInt (bigint n))) s
  let portFromDT (d : Dval) : Cap.Scope<int64> =
    fromDT (fun dv -> int64 (D.darkInt dv)) d


module HostMatch =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.hostMatch ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT (h : Cap.HostMatch) : Dval =
    let (caseName, fields) =
      match h with
      | Cap.AnyHost -> "AnyHost", []
      | Cap.ExactHost s -> "ExactHost", [ DString s ]
      | Cap.Subdomain s -> "Subdomain", [ DString s ]
    DEnum(typeName (), typeName (), [], caseName, fields)

  let fromDT (d : Dval) : Cap.HostMatch =
    match d with
    | DEnum(_, _, _, "AnyHost", []) -> Cap.AnyHost
    | DEnum(_, _, _, "ExactHost", [ DString s ]) -> Cap.ExactHost s
    | DEnum(_, _, _, "Subdomain", [ DString s ]) -> Cap.Subdomain s
    | _ -> Exception.raiseInternal "Invalid Capabilities.HostMatch" [ "dval", d ]


module UrlScope =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.urlScope ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT (u : Cap.UrlScope) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map
        [ "schemes", Scope.strToDT u.schemes
          "hosts",
          DList(
            VT.known (HostMatch.knownType ()),
            u.hosts |> List.map HostMatch.toDT
          )
          "ports", Scope.portToDT u.ports
          "paths", Scope.strToDT u.paths ]
    )

  let fromDT (d : Dval) : Cap.UrlScope =
    match d with
    | DRecord(_, _, _, f) ->
      { schemes = Scope.strFromDT (D.field "schemes" f)
        hosts = D.list HostMatch.fromDT (D.field "hosts" f)
        ports = Scope.portFromDT (D.field "ports" f)
        paths = Scope.strFromDT (D.field "paths" f) }
    | _ -> Exception.raiseInternal "Invalid Capabilities.UrlScope" [ "dval", d ]


module HttpRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.httpRule ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT (r : Cap.HttpRule) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map [ "methods", Scope.strToDT r.methods; "url", UrlScope.toDT r.url ]
    )

  let fromDT (d : Dval) : Cap.HttpRule =
    match d with
    | DRecord(_, _, _, f) ->
      { methods = Scope.strFromDT (D.field "methods" f)
        url = UrlScope.fromDT (D.field "url" f) }
    | _ -> Exception.raiseInternal "Invalid Capabilities.HttpRule" [ "dval", d ]


module ExecRule =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.execRule ())
  let knownType () = KTCustomType(typeName (), [])

  let toDT (r : Cap.ExecRule) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map [ "programs", Scope.strToDT r.programs; "args", Scope.strToDT r.args ]
    )

  let fromDT (d : Dval) : Cap.ExecRule =
    match d with
    | DRecord(_, _, _, f) ->
      { programs = Scope.strFromDT (D.field "programs" f)
        args = Scope.strFromDT (D.field "args" f) }
    | _ -> Exception.raiseInternal "Invalid Capabilities.ExecRule" [ "dval", d ]


module RW =
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Capabilities.rw ())

  let toDT (rw : Cap.RW<string>) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [ VT.string ],
      Map [ "read", Scope.strToDT rw.read; "write", Scope.strToDT rw.write ]
    )

  let fromDT (d : Dval) : Cap.RW<string> =
    match d with
    | DRecord(_, _, _, f) ->
      { read = Scope.strFromDT (D.field "read" f)
        write = Scope.strFromDT (D.field "write" f) }
    | _ -> Exception.raiseInternal "Invalid Capabilities.RW" [ "dval", d ]


module Capabilities =
  let typeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Capabilities.capabilities ()
    )
  let knownType () = KTCustomType(typeName (), [])

  let toDT (c : Cap.Capabilities) : Dval =
    DRecord(
      typeName (),
      typeName (),
      [],
      Map
        [ "httpClient",
          DList(
            VT.known (HttpRule.knownType ()),
            c.httpClient |> List.map HttpRule.toDT
          )
          "httpServer", Scope.portToDT c.httpServer
          "file", RW.toDT c.file
          "env", RW.toDT c.env
          "db", RW.toDT c.db
          "exec",
          DList(VT.known (ExecRule.knownType ()), c.exec |> List.map ExecRule.toDT)
          "stdout", DBool c.stdout
          "stdin", DBool c.stdin
          "random", DBool c.random
          "clock", DBool c.clock
          "llm", DBool c.llm ]
    )

  let fromDT (d : Dval) : Cap.Capabilities =
    match d with
    | DRecord(_, _, _, f) ->
      { httpClient = D.list HttpRule.fromDT (D.field "httpClient" f)
        httpServer = Scope.portFromDT (D.field "httpServer" f)
        file = RW.fromDT (D.field "file" f)
        env = RW.fromDT (D.field "env" f)
        db = RW.fromDT (D.field "db" f)
        exec = D.list ExecRule.fromDT (D.field "exec" f)
        stdout = D.bool (D.field "stdout" f)
        stdin = D.bool (D.field "stdin" f)
        random = D.bool (D.field "random" f)
        clock = D.bool (D.field "clock" f)
        llm = D.bool (D.field "llm" f) }
    | _ -> Exception.raiseInternal "Invalid Capabilities" [ "dval", d ]
