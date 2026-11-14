module Builtins.Pure.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError
module Interpreter = LibExecution.Interpreter
module Pending = LibExecution.Pending
module NumOps = LibExecution.NumOps


// Builtin types cannot express “both operands have the same numeric type,” so
// these operator signatures are intentionally loose. Runtime dispatch and the
// at-rest checker enforce the constraint.
let varA = TVariable "a"
let varB = TVariable "b"


/// Shared body for `equals` / `notEquals` builtins — VT-merge type
/// check + structural compare.
let private equalsBuiltinImpl (vm : VMState) (a : Dval) (b : Dval) : bool =
  let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
  match ValueType.merge vtA vtB with
  | Error _ -> RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
  | Ok _ -> Dval.equals a b


// Polymorphic numeric operators (`+`, `-`, `*`, `/`, `%`, and comparisons).
// Each inspects the runtime value type and operates on two values of the same
// numeric type, mirroring how `equals` already dispatches at runtime.

/// Raised when a numeric operator gets operands that aren't two values of the
/// same numeric type (e.g. `1L + 2.0`, `"a" + "b"`).
let private numericTypeError (vm : VMState) (a : Dval) (b : Dval) : 'a =
  RTE.NumericOperationOnIncompatibleTypes(Dval.toValueType a, Dval.toValueType b)
  |> raiseRTE vm.threadID

/// An operator's answer for anything `NumOps` has no answer for.
let private notNumbers : BuiltInFnSig =
  function
  | _, vm, _, [| a; b |] -> numericTypeError vm a b
  | _, vm, _, [| a |] -> numericTypeError vm a a
  | _ -> incorrectArgs ()

/// A polymorphic operator: `NumOps` for one or two numbers of one builtin numeric type, which is
/// where their semantics live, and <param rest> for everything else.
let private numeric (op : int) (rest : BuiltInFnSig) : BuiltInFnSig =
  fun (struct (_, vm, _, args) as call) ->
    let outcome =
      match args with
      | [| a; b |] -> NumOps.binary op a b
      | [| a |] -> NumOps.unary op a
      | _ -> NumOps.Unsupported
    match outcome with
    | NumOps.Value v -> Ply v
    | NumOps.Raises e -> raiseRTE vm.threadID e
    | NumOps.Unsupported -> rest call


/// `unwrap` of a value with no read in flight at its top.
let private unwrapLanded (vm : VMState) (dval : Dval) : Ply<Dval> =
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
    RuntimeError.Unwraps.GotNone |> RuntimeError.Unwrap |> raiseRTE vm.threadID

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


let fns () : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [| a; b |] -> equalsBuiltinImpl vm a b |> Dval.bool |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, vm, _, [| a; b |] -> equalsBuiltinImpl vm a b |> not |> Dval.bool |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Adds two values of the same type. Strings concatenate. For numbers: fixed-width "
        + "integer overflow wraps around, the arbitrary-precision BigInt grows instead of "
        + "overflowing, and float arithmetic follows IEEE (overflow to infinity)."
      fn =
        numeric NumOps.add (function
          // `+` concatenates Strings (`impl Add for String`), and this builtin is what `+`
          // lowers to while the package refs are not generated yet, which is how the stdlib's
          // own string concatenation loads on a fresh tree.
          | _, _, _, [| DString a; DString b |] -> Ply(DString(String.append a b))
          | call -> notNumbers call)
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Subtracts two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn = numeric NumOps.subtract notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Multiplies two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn = numeric NumOps.multiply notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    // TODO: make integer `/` and `%` checked by default: return a Result, and callers
    // unwrap. The interpreter may be a little slower, and the compiler should optimize
    // the unwrapping away. Until then, `checkedDivide` / `checkedMod` /
    // `checkedRemainder` on each integer type return a Result.
    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Divides two numbers of the same numeric type. Integer types use "
        + "integer division; dividing an integer by zero raises a runtime error. "
        + "Signed integer overflow (e.g. {{Int64.MinValue / -1}}) wraps around to "
        + "{{MinValue}}."
      fn = numeric NumOps.divide notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    // TODO: make integer `/` and `%` checked by default: return a Result, and callers
    // unwrap. The interpreter may be a little slower, and the compiler should optimize
    // the unwrapping away. Until then, `checkedDivide` / `checkedMod` /
    // `checkedRemainder` on each integer type return a Result.
    { name = fn "modulo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Wraps <param a> around so that {{0 <= res < b}}, for two integers of the same "
        + "type or two decimals. The modulus <param b> must be greater than 0."
      fn = numeric NumOps.modulo notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" varA ""; Param.make "exponent" varA "" ]
      returnType = varA
      description =
        "Raises a number to the power of another number of the same type. "
        + "Supported for every integer type and Float64. "
        + "A negative integer exponent raises an error. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead."
      fn = numeric NumOps.power notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    // Unary negation (`-x`). Supported for signed integer types and Float64;
    // negating the minimum signed value wraps back to itself.
    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Returns the negation of <param a>, {{-a}}"
      fn = numeric NumOps.negate notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }
    // Bitwise operators.
    { name = fn "bitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise AND of two integers of the same type"
      fn = numeric NumOps.bitwiseAnd notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise OR of two integers of the same type"
      fn = numeric NumOps.bitwiseOr notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise XOR of two integers of the same type"
      fn = numeric NumOps.bitwiseXor notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Unary `~`. Two's complement, so `~x = -x - 1` on every signed type and on
    // the arbitrary-precision `BigInt`.
    { name = fn "bitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Bitwise NOT of an integer, {{~a}}"
      fn = numeric NumOps.bitwiseNot notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "shiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise left shift of an integer by <param b> bits"
      fn = numeric NumOps.shiftLeft notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "shiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise right shift of an integer by <param b> bits"
      fn = numeric NumOps.shiftRight notNumbers
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn = numeric NumOps.greaterThan notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn = numeric NumOps.greaterThanOrEqualTo notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn = numeric NumOps.lessThan notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn = numeric NumOps.lessThanOrEqualTo notNumbers
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.moved "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [||] -> incorrectArgs ()
        | _, vm, _, [| DPromise _ as read |] ->
          // A `Some` or `Ok` built around a read hands the read inside back through its shell,
          // still in flight. Anything else is waited for: an error shows the value it got.
          match Pending.peek read with
          | DEnum(FQTypeName.Package(Hash id), _, _, "Some", [ value ]) when
            id = PackageRefs.Type.Stdlib.option ()
            ->
            Ply value
          | DEnum(FQTypeName.Package(Hash id), _, _, "Ok", [ value ]) when
            id = PackageRefs.Type.Stdlib.result ()
            ->
            Ply value
          | _ -> Interpreter.withValue vm read (fun landed -> unwrapLanded vm landed)
        | _, vm, _, [| dval |] -> unwrapLanded vm dval

        // Error: multiple arguments
        | _, vm, _, multipleArgs ->
          RuntimeError.Unwraps.MultipleArgs(List.ofArray multipleArgs)
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    // The identity: a builtin's arguments land before it runs, and that wait is the point.
    { name = fn "runtimeForce" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "a") "" ]
      returnType = TVariable "a"
      description =
        "Returns <param value>, once it is a value: a read still in flight is waited for here"
      fn =
        (function
        | _, _, _, [| value |] -> Ply value
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
