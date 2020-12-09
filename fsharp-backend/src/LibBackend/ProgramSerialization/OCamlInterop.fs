module LibBackend.ProgramSerialization.OCamlInterop

// Interoperation functions with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.  We send binary code which we get
// from the DB, convert it to OCaml types, then json convert it to get it back
// into F#. At that point we convert it to these types, and potentially convert
// it to the runtime types to run it.

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open FSharp.Data
open System.Text.RegularExpressions

open Prelude
open LibBackend.Db
open ProgramTypes
open LibExecution.SharedTypes

module Binary =
  // These allow us to call C functions from serialization_stubs.c, which in
  // turn call into the OCaml runtime.

  // FSTODO if we have segfaults, we might need to use this:
  // https://docs.microsoft.com/en-us/dotnet/standard/native-interop/best-practices#keeping-managed-objects-alive

  // initialize OCaml runtime
  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "dark_init_ocaml")>]
  extern string darkInitOcaml()

  // take a binary rep of a handler from the DB and convert it into JSON
  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "handler_bin2json")>]
  extern string handlerBin2Json(byte[] bytes, int length)

  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "handler_json2bin")>]
  extern int handlerJson2Bin(string str, System.IntPtr& byteArray)

  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "digest")>]
  extern string digest()

  let init () =
    printfn "serialization_init"
    let str = darkInitOcaml ()
    printfn "serialization_inited: %s" str
    ()


module Yojson =
  // Yojson is the OCaml automated JSON format. This module generates and
  // parses JSON in that format, so that it can be sent to/from OCaml.

  // FSTODO: if we need speed, switch to transferring using protobufs instead of
  // JSON, where do we get the protobuf files?
  // - 1) generate using ocaml types (maybe deprecated? ocaml-protoc)
  // - 2) generate using FSharp types (possible?)
  // - 3) hand-write them (converters on both ends)
  type J = JsonValue

  let variant (name : string) (args : JsonValue list) =
    (J.String name :: args) |> List.toArray |> J.Array

  let ofTLID (tlid : tlid) : JsonValue = J.Number(decimal tlid)
  let ofID (id : id) : JsonValue = J.Number(decimal id)

  let ofSendToRail (str : SendToRail) : JsonValue =
    match str with
    | Rail -> variant "Rail" []
    | NoRail -> variant "NoRail" []

  let ofFQFnName (desc : FQFnName.T) : JsonValue =
    let owner = if desc.owner = "dark" then "" else $"{desc.owner}/"
    let package = if desc.package = "stdlib" then "" else $"{desc.package}/"
    let module_ = if desc.module_ = "" then "" else $"{desc.module_}::"
    let function_ = desc.function_
    let version = if desc.version = 0 then "" else $"_v{desc.version}"
    J.String $"{owner}{package}{module_}{function_}{version}"




  let rec ofExpr (e : Expr) : JsonValue =
    let v = variant

    match e with
    | EBlank id -> v "EBlank" [ ofID id ]
    | EPartial (id, name, expr) ->
        v "EPartial" [ ofID id; J.String name; ofExpr expr ]
    | ERightPartial (id, name, expr) ->
        v "ELeftPartial" [ ofID id; J.String name; ofExpr expr ]
    | ELeftPartial (id, name, expr) ->
        v "ERightPartial" [ ofID id; J.String name; ofExpr expr ]
    | EString (id, s) -> v "EString" [ ofID id; J.String s ]
    | EVariable (id, s) -> v "EVariable" [ ofID id; J.String s ]
    | EBinOp (id, name, arg0, arg1, str) ->
        v
          "EBinOp"
          [ ofID id; ofFQFnName name; ofExpr arg0; ofExpr arg1; ofSendToRail str ]
    | _ -> failwith $"Not supported yet in ofExpr: {e}"
  // | EPipeTarget id ->
  // | ELet (_id, lhs, rhs, body) ->
  // | EBool (_id, b) ->
  // | EInteger (_id, i) ->
  // | EFloat (_id, whole, fractional) ->
  // | ENull _id ->
  // | ECharacter (_id, s) ->
  // | EList (_id, exprs) ->
  // | EVariable (_id, name) ->
  // | ERecord (id, pairs) ->
  // | EFnCall (id, desc, exprs, ster) ->
  // | EBinOp (id, desc, arg1, arg2, ster) ->
  // | EFieldAccess (id, _, _) ->
  // | EFeatureFlag (id, _, cond, oldcode, newcode) ->
  // | ELambda (_id, parameters, body) ->
  // | EPipe (id, e1, e2, rest) ->
  // | EMatch (id, matchExpr, cases) ->
  //       match pattern with
  //       | PInteger (pid, i) ->
  //       | PBool (pid, bool) ->
  //       | PCharacter (pid, c) ->
  //       | PString (pid, str) ->
  //       | PFloat (pid, whole, fraction) ->
  //       | PNull (pid) ->
  //       | PVariable (pid, v) ->
  //       | PBlank (_pid) ->
  //       | PConstructor (pid, name, args) ->
  // | EIf (_id, cond, thenbody, elsebody) ->
  // | EConstructor (id, name, args) ->
  //

  let ofHandler (h : Handler.T) : JsonValue =
    // { module_ : string or_blank [@key "module"]
    // ; name : string or_blank
    // ; modifier : string or_blank
    // ; types : spec_types }

    let spec =
      let blankOr (id : id) (v : string) : JsonValue =
        if v = "" then
          J.Array [| J.String "Blank"; ofID id |]
        else
          J.Array [| J.String "Filled"; ofID id; J.String v |]

      // deprecated but still needed :( to be the right shape
      let specTypes =
        J.Record [| "input", blankOr (gid ()) ""; "output", blankOr (gid ()) "" |]

      match h.spec with
      | Handler.REPL (name, ids) ->
          J.Record [| "module", blankOr ids.moduleID "REPL"
                      "name", blankOr ids.nameID name
                      "modifier", blankOr ids.modifierID "_"
                      "types", specTypes |]
      | Handler.HTTP (path, modifier, ids) ->
          J.Record [| "module", blankOr ids.moduleID "HTTP"
                      "name", blankOr ids.nameID path
                      "modifier", blankOr ids.modifierID modifier
                      "types", specTypes |]
      | _ -> failwith $"More to handle in ofHandler {h.spec}"

    let result =
      J.Record [| "tlid", ofTLID h.tlid; "ast", ofExpr h.ast; "spec", spec |]

    result


  let rec toExpr (j : JsonValue) : Expr =

    let e = toExpr
    let es exprs = exprs |> Array.map e |> Array.toList

    let toRail (j : JsonValue) : SendToRail =
      match j with
      | J.Array [| J.String "Rail" |] -> Rail
      | J.Array [| J.String "NoRail" |] -> NoRail
      | _ -> failwith $"Unimplemented {j}"

    let rec toPattern (j : JsonValue) : Pattern =
      let constructor = j.Item(0).AsString()
      if constructor = "FPString" then
        // FPString uses a different format than the others
        let obj = j.Item(1)
        PString(obj.Item("patternID").AsInteger64(), obj.Item("str").AsString())
      else
        // skip the matchID in j.Item(1), we know from context
        let id = j.Item(2).AsInteger64()
        match constructor, j.AsArray().[3..] |> Array.toList with
        | "FPBlank", _ -> PBlank id
        | "FPInteger", [ J.String i ] -> PInteger(id, i)
        | "FPBool", [ J.Boolean bool ] -> PBool(id, bool)
        | "FPFloat", [ J.String whole; J.String fraction ] ->
            PFloat(id, whole, fraction)
        | "FPNull", _ -> PNull id
        | "FPVariable", [ J.String name ] -> PVariable(id, name)
        | "FPConstructor", [ J.String name; J.Array pats ] ->
            PConstructor(id, name, Array.map toPattern pats |> Array.toList)

        | _ -> failwith $"Unimplemented {j}"




    let constructor = j.Item(0).AsString()
    let id = j.Item(1).AsInteger64()
    match constructor, j.AsArray().[2..] |> Array.toList with
    | "EString", [ J.String str ] -> EString(id, str)
    | "EInteger", [ J.String i ] -> EInteger(id, i)
    | "EBool", [ J.Boolean bool ] -> EBool(id, bool)
    | "EFloat", [ J.String whole; J.String fraction ] -> EFloat(id, whole, fraction)
    | "ENull", _ -> ENull id
    | "EBlank", _ -> EBlank(id)
    | "EPipeTarget", _ -> EPipeTarget(id)
    | "EVariable", [ J.String var ] -> EVariable(id, var)
    | "EList", [ J.Array exprs ] -> EList(id, es exprs)
    | "ELet", [ J.String name; rhs; body ] -> ELet(id, name, e rhs, e body)
    | "EIf", [ cond; thenBody; elseBody ] -> EIf(id, e cond, e thenBody, e elseBody)

    | "EPipe", [ J.Array exprs ] ->
        match es exprs with
        | [] -> failwith "empty pipe"
        | e :: [] -> e
        | e1 :: e2 :: rest -> EPipe(id, e1, e2, rest)
    | "ERecord", [ J.Array rows ] ->
        let rows =
          rows
          |> Array.toList
          |> List.map (fun (row : JsonValue) ->
               match row with
               | J.Array [| J.String key; value |] -> (key, e value)
               | _ -> failwith "unexpected record row shape")

        ERecord(id, rows)
    | "EMatch", [ cond; J.Array rows ] ->
        let rows =
          rows
          |> Array.toList
          |> List.map (fun (row : JsonValue) ->
               match row with
               | J.Array [| pat; value |] -> (toPattern pat, e value)
               | _ -> failwith "unexpected record row shape")

        EMatch(id, e cond, rows)


    | "EConstructor", [ J.String name; J.Array exprs ] ->
        EConstructor(id, name, es exprs)

    // FSTODO: sometimes these are binops, probably cause we don't load the
    // library in OCaml. Nevermind, we can do it over here.
    | "EFnCall", [ J.String name; J.Array args; rail ] ->
        let desc =
          match name with
          | Regex "(.+)/(.+)/(.+)::(.+)_v(\d+)"
                  [ owner; package; module'; name; version ] ->
              FQFnName.name owner package module' name (int version)
          | Regex "(.+)::(.+)_v(\d+)" [ module'; name; version ] ->
              FQFnName.stdlibName module' name (int version)
          | Regex "(.+)::(.+)" [ module'; name ] ->
              FQFnName.stdlibName module' name 0
          | Regex "(.+)_v(\d+)" [ name; version ] ->
              FQFnName.stdlibName "" name (int version)
          | Regex "(.+)" [ name ] -> FQFnName.stdlibName "" name 0
          | _ -> failwith $"Bad format in function name: \"{name}\""

        EFnCall(id, desc, Array.map e args |> Array.toList, toRail rail)
    | _ -> failwith $"Unimplemented expr {j}"

  let toHandler (j : JsonValue) : Handler.T =
    let toHandlerSpec (j : JsonValue) : Handler.Spec =
      let toBlankOr (j : JsonValue) : (id * string) =
        // element 0 is "filled" or "blank"
        let value =
          j.AsArray()
          |> Array.tryItem 2
          |> Option.map (fun j -> j.AsString())
          |> Option.defaultValue ""

        (j.Item(1).AsInteger64(), value)

      let moduleID, module_ = j.Item("module") |> toBlankOr
      let nameID, name = j.Item("name") |> toBlankOr
      let modifierID, modifier = j.Item("modifier") |> toBlankOr

      let (ids : Handler.ids) =
        { moduleID = moduleID; nameID = nameID; modifierID = modifierID }

      if module_ = "" then
        Handler.REPL(name = name, ids = ids)
      else if module_ = "REPL" then
        Handler.REPL(name = name, ids = ids)
      else if module_ = "HTTP" then
        Handler.HTTP(path = name, method = modifier, ids = ids)
      else if module_ = "CRON" then
        Handler.Cron(name = name, interval = modifier, ids = ids)
      else if module_ = "WORKER" then
        Handler.Worker(name = name, ids = ids)
      else
        Handler.OldWorker(modulename = module_, name = name, ids = ids)

    { tlid = j.Item("tlid").AsInteger64()
      ast = j.Item("ast") |> toExpr
      spec = j.Item("spec") |> toHandlerSpec }

let toplevelOfCachedBinary ((data, pos) : (byte array * string option)) : Toplevel =
  Binary.handlerBin2Json (data, data.Length)
  |> FSharp.Data.JsonValue.Parse
  |> Yojson.toHandler
  |> TLHandler

let toplevelToCachedBinary (toplevel : Toplevel) : byte array =
  match toplevel with
  | TLHandler h ->
      let json = (Yojson.ofHandler h).ToString()
      let mutable destPtr = System.IntPtr()
      let length = Binary.handlerJson2Bin (json, &destPtr)
      let mutable (bytes : byte array) = Array.zeroCreate length
      Marshal.Copy(destPtr, bytes, 0, length)
      bytes

  | _ -> failwith $"toplevel not supported yet {toplevel}"
