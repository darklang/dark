module BuiltinExecution.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module ValueType = LibExecution.ValueType
module Exe = LibExecution.Execution
module RTE = RuntimeError


/// State-aware structural equality. Walks two Dvals in parallel and
/// returns true iff every reachable leaf compares equal. Type errors
/// (callers passing structurally-incompatible Dvals) return false
/// rather than raising — the caller's responsibility to type-check
/// up front via VT.merge.
///
/// Blobs are compared by content, not handle: same-hash Persistents
/// are cheap; cross-handle ephemerals (and ephemeral-vs-persistent)
/// dereference and byte-compare. No `Blob.promote` side effect, no
/// Dval-tree rebuild — both of which the previous implementation did
/// on the wrapper side.
///
/// Streams compare by reference identity on their `lockObj`. Same
/// handle → true (preserves reflexivity). Different handles → false;
/// cross-handle equality would require draining, which violates
/// single-consumer semantics. (Conservative pick — raising "cannot
/// compare streams" is also defensible; revisit if anyone relies on
/// the always-false behaviour.)
let rec equals (state : ExecutionState) (a : Dval) (b : Dval) : Ply.Ply<bool> =
  let r = equals state

  uply {
    match a, b with
    | DUnit, DUnit -> return true

    | DBool a, DBool b -> return a = b

    | DInt8 a, DInt8 b -> return a = b
    | DUInt8 a, DUInt8 b -> return a = b
    | DInt16 a, DInt16 b -> return a = b
    | DUInt16 a, DUInt16 b -> return a = b
    | DInt32 a, DInt32 b -> return a = b
    | DUInt32 a, DUInt32 b -> return a = b
    | DInt64 a, DInt64 b -> return a = b
    | DUInt64 a, DUInt64 b -> return a = b
    | DInt128 a, DInt128 b -> return a = b
    | DUInt128 a, DUInt128 b -> return a = b

    | DFloat a, DFloat b -> return a = b

    | DChar a, DChar b -> return a = b
    | DString a, DString b -> return a = b

    | DDateTime a, DDateTime b -> return a = b

    | DUuid a, DUuid b -> return a = b

    | DList(typA, a), DList(typB, b) ->
      if Result.isError (ValueType.merge typA typB) || a.Length <> b.Length then
        return false
      else
        let mutable allEqual = true
        let mutable i = 0
        while allEqual && i < a.Length do
          let! eq = r a[i] b[i]
          if not eq then allEqual <- false
          i <- i + 1
        return allEqual

    | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
      if a3.Length <> b3.Length then
        return false
      else
        let! e1 = r a1 b1
        if not e1 then
          return false
        else
          let! e2 = r a2 b2
          if not e2 then
            return false
          else
            let mutable allEqual = true
            let mutable i = 0
            while allEqual && i < a3.Length do
              let! eq = r a3[i] b3[i]
              if not eq then allEqual <- false
              i <- i + 1
            return allEqual

    | DDict(typeA, a), DDict(typeB, b) ->
      if
        Result.isError (ValueType.merge typeA typeB) || Map.count a <> Map.count b
      then
        return false
      else
        let mutable allEqual = true
        let entries = Map.toList a
        let mutable i = 0
        while allEqual && i < entries.Length do
          let (k, va) = entries[i]
          match Map.find k b with
          | None -> allEqual <- false
          | Some vb ->
            let! eq = r va vb
            if not eq then allEqual <- false
          i <- i + 1
        return allEqual

    | DRecord(_, typeNameA, typeArgsA, fieldsA),
      DRecord(_, typeNameB, typeArgsB, fieldsB) ->
      if
        typeNameA <> typeNameB
        || typeArgsA.Length <> typeArgsB.Length
        || not (
          List.forall2
            (fun ta tb -> Result.isOk (ValueType.merge ta tb))
            typeArgsA
            typeArgsB
        )
        || Map.count fieldsA <> Map.count fieldsB
      then
        return false
      else
        let mutable allEqual = true
        let entries = Map.toList fieldsA
        let mutable i = 0
        while allEqual && i < entries.Length do
          let (k, va) = entries[i]
          match Map.find k fieldsB with
          | None -> allEqual <- false
          | Some vb ->
            let! eq = r va vb
            if not eq then allEqual <- false
          i <- i + 1
        return allEqual

    | DEnum(_, typeNameA, typeArgsA, caseNameA, fieldsA),
      DEnum(_, typeNameB, typeArgsB, caseNameB, fieldsB) ->
      if
        typeNameA <> typeNameB
        || typeArgsA.Length <> typeArgsB.Length
        || not (
          List.forall2
            (fun ta tb -> Result.isOk (ValueType.merge ta tb))
            typeArgsA
            typeArgsB
        )
        || caseNameA <> caseNameB
        || fieldsA.Length <> fieldsB.Length
      then
        return false
      else
        let mutable allEqual = true
        let mutable i = 0
        while allEqual && i < fieldsA.Length do
          let! eq = r fieldsA[i] fieldsB[i]
          if not eq then allEqual <- false
          i <- i + 1
        return allEqual

    | DApplicable a, DApplicable b ->
      match a, b with
      // CLEANUP exprId is a partial check — fully checking LambdaImpl
      // equality needs lambda-internal-state work. Today this is
      // "same-source-position lambdas compare equal."
      | AppLambda a, AppLambda b -> return a.exprId = b.exprId
      | AppNamedFn a, AppNamedFn b -> return a = b
      | _ -> return false

    | DDB a, DDB b -> return a = b

    | DBlob refA, DBlob refB ->
      // Cheap hash-compare for two Persistents; same-uuid for same-handle
      // ephemerals; otherwise dereference and byte-compare.
      match refA, refB with
      | Persistent(h1, l1), Persistent(h2, l2) -> return h1 = h2 && l1 = l2
      | Ephemeral id1, Ephemeral id2 when id1 = id2 -> return true
      | _ ->
        let! bytesA = Blob.readBytes state refA
        let! bytesB = Blob.readBytes state refB
        return bytesA = bytesB

    | DStream(_, _, lockA), DStream(_, _, lockB) ->
      // Reference equality on lockObj — same-handle preserves
      // reflexivity. Cross-handle compare without consuming the streams
      // is fundamentally impossible under the single-consumer rule.
      return System.Object.ReferenceEquals(lockA, lockB)

    // exhaustiveness — type mismatches return false; caller VT-merges
    // up front to convert to a clean RTE.
    | DUnit, _
    | DBool _, _
    | DInt8 _, _
    | DUInt8 _, _
    | DInt16 _, _
    | DUInt16 _, _
    | DInt32 _, _
    | DUInt32 _, _
    | DInt64 _, _
    | DUInt64 _, _
    | DInt128 _, _
    | DUInt128 _, _
    | DFloat _, _
    | DChar _, _
    | DString _, _
    | DDateTime _, _
    | DUuid _, _
    | DList _, _
    | DTuple _, _
    | DDict _, _
    | DRecord _, _
    | DEnum _, _
    | DApplicable _, _
    | DDB _, _
    | DBlob _, _
    | DStream _, _ -> return false
  }


let varA = TVariable "a"
let varB = TVariable "b"


/// Shared body for `equals` / `notEquals` builtins — VT-merge type
/// check + structural compare.
let private equalsBuiltinImpl
  (state : ExecutionState)
  (vm : VMState)
  (a : Dval)
  (b : Dval)
  : Ply.Ply<bool> =
  let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
  match ValueType.merge vtA vtB with
  | Error _ -> RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
  | Ok _ -> equals state a b


let fns () : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | state, vm, _, [ a; b ] ->
          uply {
            let! result = equalsBuiltinImpl state vm a b
            return DBool result
          }
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | state, vm, _, [ a; b ] ->
          uply {
            let! result = equalsBuiltinImpl state vm a b
            return DBool(not result)
          }
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [] -> incorrectArgs ()
        | _, vm, _, [ dval ] ->
          match dval with

          // Success: extract `Some` out of an Option
          | DEnum(FQTypeName.Package(Hash id), _, _, "Some", [ value ]) when
            id = PackageRefs.Type.Stdlib.option ()
            ->
            Ply value

          // Success: extract `Ok` out of a Result
          | DEnum(FQTypeName.Package(Hash id), _, _, "Ok", [ value ]) when
            id = PackageRefs.Type.Stdlib.result ()
            ->
            Ply value

          // Error: expected Some, got None
          | DEnum(FQTypeName.Package(Hash id), _, _, "None", []) when
            id = PackageRefs.Type.Stdlib.option ()
            ->
            RuntimeError.Unwraps.GotNone
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

          // Error: expected Ok, got Error
          | DEnum(FQTypeName.Package(Hash id), _, _, "Error", [ value ]) when
            id = PackageRefs.Type.Stdlib.result ()
            ->
            RuntimeError.Unwraps.GotError value
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

          // Error: single dval, but not an Option or Result
          | otherDval ->
            RuntimeError.Unwraps.NonOptionOrResult otherDval
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

        // Error: multiple arguments
        | _, vm, _, multipleArgs ->
          RuntimeError.Unwraps.MultipleArgs multipleArgs
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "debug" 0
      typeParams = []
      parameters =
        [ Param.make "label" TString "The label to be printed."
          Param.make "value" (TVariable "a") "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output"
      fn =
        (function
        | exeState, _, _, [ DString label; value ] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            print $"DEBUG: {label}: {repr}"
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    /// CLEANUP not sure why we need this - feels like an extra step. (package -> builtin -> package)
    { name = fn "toRepr" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "a") "The value to convert." ]
      returnType = TString
      description = "Returns a string representation of the given <param value>"
      fn =
        (function
        | exeState, _, _, [ value ] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            return DString repr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
