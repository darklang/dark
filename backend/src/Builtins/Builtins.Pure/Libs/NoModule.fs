module Builtins.Pure.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module Exe = LibExecution.Execution
module RTE = RuntimeError


/// Structural equality. Walks two Dvals in parallel and returns
/// true iff every reachable leaf compares equal. Type errors
/// (callers passing structurally-incompatible Dvals) return false
/// rather than raising — the caller's responsibility to type-check
/// up front via VT.merge.
///
/// Blob comparison is identity-based: same-hash Persistents are
/// equal; same-UUID Ephemerals are equal; mixed cases (different
/// Ephemerals, Ephemeral vs Persistent) are false even when bytes
/// match. Byte-equality across freshly-built ephemerals would
/// require dereferencing — for Persistents that's an async
/// `package_blobs` lookup, which would force the entire `equals`
/// surface to be Ply-shaped just for one rare case. Callers that
/// want byte-equality across ephemerals should `Blob.promote` both
/// sides first; same bytes → same hash → equal as Persistents.
///
/// Streams compare by reference identity on their `lockObj`. Same
/// handle → true (preserves reflexivity). Different handles → false;
/// cross-handle equality would require draining, which violates
/// single-consumer semantics.
let rec equals (a : Dval) (b : Dval) : bool =
  let r = equals

  match a, b with
  | DUnit, DUnit -> true

  | DBool a, DBool b -> a = b

  | DInt8 a, DInt8 b -> a = b
  | DUInt8 a, DUInt8 b -> a = b
  | DInt16 a, DInt16 b -> a = b
  | DUInt16 a, DUInt16 b -> a = b
  | DInt32 a, DInt32 b -> a = b
  | DUInt32 a, DUInt32 b -> a = b
  | DInt64 a, DInt64 b -> a = b
  | DUInt64 a, DUInt64 b -> a = b
  | DInt128 a, DInt128 b -> a = b
  | DUInt128 a, DUInt128 b -> a = b

  | DFloat a, DFloat b -> a = b

  | DChar a, DChar b -> a = b
  | DString a, DString b -> a = b

  | DDateTime a, DDateTime b -> a = b

  | DUuid a, DUuid b -> a = b

  | DList(typA, a), DList(typB, b) ->
    Result.isOk (ValueType.merge typA typB)
    && a.Length = b.Length
    && List.forall2 r a b

  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    a3.Length = b3.Length && r a1 b1 && r a2 b2 && List.forall2 r a3 b3

  | DDict(typeA, a), DDict(typeB, b) ->
    Result.isOk (ValueType.merge typeA typeB)
    && Map.count a = Map.count b
    && (a
        |> Map.toSeq
        |> Seq.forall (fun (k, va) ->
          match Map.find k b with
          | Some vb -> r va vb
          | None -> false))

  | DRecord(_, typeNameA, typeArgsA, fieldsA),
    DRecord(_, typeNameB, typeArgsB, fieldsB) ->
    typeNameA = typeNameB
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun ta tb -> Result.isOk (ValueType.merge ta tb))
      typeArgsA
      typeArgsB
    && Map.count fieldsA = Map.count fieldsB
    && (fieldsA
        |> Map.toSeq
        |> Seq.forall (fun (k, va) ->
          match Map.find k fieldsB with
          | Some vb -> r va vb
          | None -> false))

  | DEnum(_, typeNameA, typeArgsA, caseNameA, fieldsA),
    DEnum(_, typeNameB, typeArgsB, caseNameB, fieldsB) ->
    typeNameA = typeNameB
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun ta tb -> Result.isOk (ValueType.merge ta tb))
      typeArgsA
      typeArgsB
    && caseNameA = caseNameB
    && fieldsA.Length = fieldsB.Length
    && List.forall2 r fieldsA fieldsB

  | DApplicable a, DApplicable b ->
    match a, b with
    // CLEANUP exprId is a partial check — fully checking LambdaImpl
    // equality needs lambda-internal-state work. Today this is
    // "same-source-position lambdas compare equal."
    | AppLambda a, AppLambda b -> a.exprId = b.exprId
    | AppNamedFn a, AppNamedFn b -> a = b
    | _ -> false

  | DDB a, DDB b -> a = b

  | DBlob refA, DBlob refB ->
    // Identity-based: same hash (Persistent) or same UUID (Ephemeral).
    // Different ephemerals never compare equal — promote first if you
    // want byte-equality.
    match refA, refB with
    | Persistent(h1, l1), Persistent(h2, l2) -> h1 = h2 && l1 = l2
    | Ephemeral id1, Ephemeral id2 -> id1 = id2
    | _ -> false

  | DStream(_, _, lockA), DStream(_, _, lockB) ->
    // Reference equality on lockObj — same-handle preserves
    // reflexivity. Cross-handle compare without consuming the streams
    // is fundamentally impossible under the single-consumer rule.
    System.Object.ReferenceEquals(lockA, lockB)

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
  | DStream _, _ -> false


let varA = TVariable "a"
let varB = TVariable "b"


/// Shared body for `equals` / `notEquals` builtins — VT-merge type
/// check + structural compare.
let private equalsBuiltinImpl (vm : VMState) (a : Dval) (b : Dval) : bool =
  let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
  match ValueType.merge vtA vtB with
  | Error _ -> RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
  | Ok _ -> equals a b


let fns () : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [ a; b ] -> equalsBuiltinImpl vm a b |> DBool |> Ply
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
        | _, vm, _, [ a; b ] -> equalsBuiltinImpl vm a b |> not |> DBool |> Ply
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
