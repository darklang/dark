/// Reusable errors and messages used mostly in the standard library
module LibExecution.Errors

open Prelude
open RuntimeTypes

// ------------------
// Exception types
// ------------------
// We have a number of special exceptions that are used as control-flow to jump out
// of Stdlib execution.
// ------------------

/// Standard error raised for calling a function with arguments of an incorrect type
exception IncorrectArgs

/// When we encounter a fakeDval, this exception allows us to jump out of the
/// computation immediately, and the caller can return the dval. This is useful
/// for jumping out of folds and other complicated constructs.
exception FakeDvalFound of Dval


// ------------------
// Messages
// ------------------
let expectedLambdaType
  (fnName : string)
  (typ : TypeReference)
  (actual : Dval)
  : string =
  let actual = DvalReprDeveloper.toRepr actual
  let typ = DvalReprDeveloper.typeName typ
  $"Expected `{fnName}` to return a {typ}, but it returned `{actual}`"

let expectedLambdaValue
  (fnName : string)
  (expected : string)
  (actual : Dval)
  : string =
  let actual = DvalReprDeveloper.toRepr actual
  $"Expected `{fnName}` to return {expected}, but it returned `{actual}`"


/// Used for values which are outside the range of expected values for some
/// reason. Really, any function using this should have a Result type instead.
let argumentWasnt (expected : string) (paramName : string) (dv : Dval) : string =
  let actual = DvalReprDeveloper.toRepr dv
  $"Expected `{paramName}` to be {expected}, but it was `{actual}`"

/// Used for lists which contain invalid values for some reason.
let argumentWasntType
  (expected : TypeReference)
  (paramName : string)
  (dv : Dval)
  : string =
  let actual = DvalReprDeveloper.toRepr dv
  let expected = DvalReprDeveloper.typeName expected
  $"Expected `{paramName}` to be a `{expected}`, but it was `{actual}`"

let resultWasntType (expected : TypeReference) (dv : Dval) : string =
  let actual = DvalReprDeveloper.toRepr dv
  let expected = DvalReprDeveloper.typeName expected
  $"Expected result to be a `{expected}`, but it was `{actual}`"

let typeErrorMsg
  (colName : string)
  (expected : TypeReference)
  (actual : Dval)
  : string =
  let expected = DvalReprDeveloper.typeName expected
  let actualType = DvalReprDeveloper.dvalTypeName actual

  $"Expected a value of type {expected} but got a {actualType} (`{actual}`)"
  + $" in column {colName}"

// ------------------
// Extremely common exceptions
// ------------------

/// When a function in called with the wrong number of arguments.
/// Used in almost every function signature.
let incorrectArgs () = raise IncorrectArgs

let intInfixFns = Set [ "+"; "-"; "*"; ">"; ">="; "<="; "<"; "^"; "%" ]

let incorrectArgsMsg (name : FnName.T) (p : Param) (actual : Dval) : string =
  let actualRepr = DvalReprDeveloper.toRepr actual
  let expectedTypeRepr = DvalReprDeveloper.typeName p.typ

  let conversionMsg =
    match p.typ, actual, name with
    | TInt, DFloat _, FQName.BuiltIn({ name = FnName.FnName name } as b) when
      b.modules = [ "Int" ] || (b.modules = [] && Set.contains name intInfixFns)
      ->
      let altfn = { b with modules = [ "Float" ] }

      $". Try using {FnName.builtinToString altfn}, or use Float.truncate to truncate Floats to Ints."
    | TInt, DString _, FQName.BuiltIn({ name = FnName.FnName name } as b) when
      (b.modules = [ "Int" ] && name = "add") || (b.modules = [] && name = "+")
      ->
      ". Use ++ to concatenate"
    | _ -> ""
  $"{FnName.toString name} was expected to be called with a `{expectedTypeRepr}`"
  + $" in {p.name}, but was actually called with {actualRepr}"
  + conversionMsg

let incorrectArgsToDError (source : DvalSource) (fn : Fn) (argList : List<Dval>) =
  let paramLength = List.length fn.parameters
  let argLength = List.length argList

  if paramLength <> argLength then
    (Dval.errSStr
      source
      ($"{FnName.toString fn.name} has {paramLength} parameters,"
       + $" but here was called with {argLength} arguments."))

  else
    let invalid =
      List.zip fn.parameters argList
      |> List.filter (fun (p, a) -> not (Dval.typeMatches p.typ a))

    match invalid with
    | [] ->
      Dval.errSStr
        source
        $"unknown error calling {FnName.toString fn.name}, with args {argList} and params {fn.parameters}"
    | (p, actual) :: _ ->
      let msg = incorrectArgsMsg fn.name p actual
      Dval.errSStr source msg


/// When a function has been removed (rarely happens but does happen occasionally)
let removedFunction (state : ExecutionState) (fnName : string) : DvalTask =
  state.notify state "function removed" [ "fnName", fnName ]
  Ply(DError(SourceNone, $"{fnName} was removed from Dark"))

/// When you have a fakeval, you typically just want to return it.
let foundFakeDval (dv : Dval) : 'a = raise (FakeDvalFound dv)

/// ---------------
/// Error pretty printing
/// CLEANUP: should be moved into Dark code ASAP, and we should return error types instead
/// ---------------
/// Segments allow us to build error messages where the UI and CLI can both
/// decorate/link to the sources in a native way
type ErrorSegment =
  | String of string
  | Int of int
  | Ordinal of int // 1st, 2nd, etc
  | FunctionName of FnName.T
  | Description of string // description from StdLib description fields. Has markers like <param name>, that should be parsed and displayed (TODO: why parse?)
  | ParamName of string
  | TypeName of TypeName.T
  | TypeReference of TypeReference
  | TypeOfValue of Dval
  | IndefiniteArticle // "a" or "an"
  | InlineValue of Dval // possibly shortened
  | FullValue of Dval

module ErrorSegment =
  let toString (list : List<ErrorSegment>) : string =
    list
    |> List.rev
    |> List.fold
      (fun prevSegments segment ->
        let newSegment =
          match segment with
          | String s -> s
          | Int i -> string i
          | Ordinal i -> String.toOrdinal i
          | IndefiniteArticle ->
            match List.tryHead prevSegments with
            | None -> ""
            | Some prev -> String.articleFor prev + " "
          | FunctionName fn -> FnName.toString fn
          | Description d -> d
          | ParamName p -> p
          | TypeName t -> TypeName.toString t
          | TypeReference t -> DvalReprDeveloper.typeName t
          | TypeOfValue dv -> DvalReprDeveloper.dvalTypeName dv
          | InlineValue dv ->
            DvalReprDeveloper.toRepr dv |> String.truncateWithElipsis 10
          | FullValue dv -> DvalReprDeveloper.toRepr dv
        newSegment :: prevSegments)
      []
    |> String.concat ""

type ErrorOutput =
  { summary : List<ErrorSegment>
    // Summary can be used on its own or concatenated with extraExplanation
    extraExplanation : List<ErrorSegment>
    actual : List<ErrorSegment>
    expected : List<ErrorSegment> }

type Error = TypeError of TypeChecker.Error
module TCK = TypeChecker

let toSegments (e : Error) : ErrorOutput =
  match e with
  | TypeError(TCK.ValueNotExpectedType(argument,
                                       expected,
                                       TCK.FunctionCallParameter(fnName,
                                                                 parameter,
                                                                 paramIndex,
                                                                 location))) ->

    let (tlid, id) = location |> Option.defaultValue (0UL, 0UL)
    let summary =
      [ FunctionName fnName
        String "'s "
        Ordinal(paramIndex + 1)
        String " argument ("
        ParamName parameter.name
        String ") should be "
        IndefiniteArticle
        TypeReference parameter.typ ]

    let extraExplanation =
      [ String ". However, "
        IndefiniteArticle
        TypeOfValue argument
        String " ("
        InlineValue argument
        String ") was passed instead." ]

    let actual =
      [ IndefiniteArticle; TypeOfValue argument; String ": "; FullValue argument ]

    // (json : string) // parameter
    let comment =
      if "" (* TODO parameter.comment*) = "" then
        []
      else
        [ String " // "; Description "" (*parameter.comment*) ]
    let expected =
      [ String "("
        ParamName parameter.name

        String ": "
        TypeReference parameter.typ
        String ")" ]
      @ comment

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }
  | _ ->
    { summary = [ String "TODO: support formatting this error message" ]
      extraExplanation = [ String(string e) ]
      actual = []
      expected = [] }

let toString (e : Error) : string =
  let s = toSegments e
  let explanation = ErrorSegment.toString (s.summary @ s.extraExplanation)
  let actual = ErrorSegment.toString s.actual
  let expected = ErrorSegment.toString s.expected

  $"{explanation}\n\nExpected: {expected}\nActual: {actual}"
