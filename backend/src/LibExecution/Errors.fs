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
  let actualType = actual |> DvalReprDeveloper.toTypeName

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
  // -- Basic types
  | String of string
  | Int of int
  | Ordinal of int // 1st, 2nd, etc
  | IndefiniteArticle // "a" or "an" (chosen based on the next segment)

  // -- Functions
  | FunctionName of FnName.T
  /// Description from StdLib description fields.
  /// Has markers like <param name>, that should be parsed and displayed (TODO: why parse?)
  | Description of string
  | ParamName of string
  | InlineParamName of string

  // -- Types
  | TypeName of TypeName.T
  | ShortTypeName of TypeName.T // Just the last bit of the TypeName, eg Result vs PACKAGE.Darklang.Stdlib.Result.Result
  | TypeReference of TypeReference
  | TypeOfValue of Dval
  | FieldName of string // records and enums
  | InlineFieldName of string // records and enums


  // -- Variables
  | DBName of string
  | VarName of string
  | InlineVarName of string

  // -- Dvals
  | InlineValue of Dval // possibly shortened to be shown inline
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
          | ParamName p -> "`" + p + "`"
          // Inline versions don't have quotes
          | InlineParamName p -> p
          | TypeName t -> TypeName.toString t
          | ShortTypeName t -> TypeName.toShortName t
          | TypeReference t -> DvalReprDeveloper.typeName t
          | TypeOfValue dv -> dv |> DvalReprDeveloper.toTypeName
          | FieldName f -> "`" + f + "`"
          | InlineFieldName f -> f
          | DBName db -> db
          | VarName v -> "`" + v + "`"
          | InlineVarName v -> v
          | InlineValue dv ->
            DvalReprDeveloper.toRepr dv
            |> String.truncateWithElipsis 10
            |> String.splitOnNewline
            |> String.concat ""
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

module NameResolution =
  type ErrorType =
    | NotFound
    | MissingModuleName
    | InvalidPackageName

  type NameType =
    | Function
    | Type
    | Constant

  type Error =
    { errorType : ErrorType
      nameType : NameType
      // The `.`-delimited name _parts_ e.g. `List.fakeFunction` is `["List";
      // "fakeFunction"]`
      names : List<string> }


type Error =
  | TypeError of TypeChecker.Error
  | NameResolutionError of NameResolution.Error

module TCK = TypeChecker

let rec valuePath (context : TCK.Context) : string =
  match context with
  | TCK.FunctionCallParameter(_, parameter, _, _) -> parameter.name
  | TCK.FunctionCallResult(_, _, _) -> "result"
  | TCK.RecordField(_, fieldName, _, _) -> fieldName
  | TCK.DictKey(key, _, _) -> $".{key}"
  | TCK.EnumField(_, caseName, _, _, _, _) -> caseName
  | TCK.DBSchemaType(dbName, _, _) -> dbName
  | TCK.DBQueryVariable(varName, _, _) -> varName
  | TCK.ListIndex(index, _, parent) -> valuePath parent + $"[{index}]"
  | TCK.TupleIndex(index, _, parent) -> valuePath parent + $"[{index}]"

let rec rootContext (context : TCK.Context) : TCK.Context =
  match context with
  | TCK.FunctionCallParameter _ -> context
  | TCK.FunctionCallResult _ -> context
  | TCK.RecordField _ -> context
  | TCK.DictKey _ -> context
  | TCK.EnumField _ -> context
  | TCK.DBSchemaType _ -> context
  | TCK.DBQueryVariable _ -> context
  | TCK.ListIndex(_, _, parent) -> rootContext parent
  | TCK.TupleIndex(_, _, parent) -> rootContext parent


// Return the segments describing the context as a short name, used in the description of errors
let rec contextSummary (context : TCK.Context) : List<ErrorSegment> =
  match context with
  | TCK.FunctionCallParameter(fnName, parameter, paramIndex, _) ->
    [ FunctionName fnName
      String "'s "
      Ordinal(paramIndex + 1)
      String " argument ("
      ParamName parameter.name
      String ")" ]
  | TCK.FunctionCallResult(fnName, returnType, _) ->
    [ FunctionName fnName; String "'s return value" ]
  | TCK.RecordField(recordType, fieldName, _, _) ->
    [ TypeName recordType; String "'s "; FieldName fieldName; String " field" ]
  | TCK.DictKey(key, _, _) ->
    let typeName =
      FQName.BuiltIn { name = TypeName.TypeName "Dict"; modules = []; version = 0 }
    [ TypeName typeName; String "'s "; FieldName key; String " value" ]
  | TCK.EnumField(enumType, caseName, fieldIndex, _, _, _) ->
    [ TypeName enumType
      String "."
      InlineFieldName caseName
      String "'s "
      Ordinal(fieldIndex + 1)
      String " argument" ]

  | TCK.DBSchemaType(dbName, expectedType, _) ->
    [ String "DB "; DBName dbName; String "'s value" ]
  | TCK.DBQueryVariable(varName, _, _) -> [ String "Variable "; VarName varName ]
  | TCK.TupleIndex(index, typ, parent)
  | TCK.ListIndex(index, typ, parent) ->
    let rootContext = rootContext parent
    [ String "In " ]
    @ contextSummary rootContext
    @ [ String ", the nested value "; VarName(valuePath context) ]

let rec contextAsActualExpected
  (argument : Dval)
  (context : TCK.Context)
  : List<ErrorSegment> * List<ErrorSegment> =

  // TODO: We do actual and expected in the same function so that we can display
  // them the same way. This hasn't been ported for all Context types, but
  // should be.
  let defaultActual =
    [ IndefiniteArticle; TypeOfValue argument; String ": "; FullValue argument ]

  match context with
  | TCK.FunctionCallParameter(fnName, parameter, paramIndex, _) ->
    // format:
    // (json : string) // some description
    let comment =
      if "" (* TODO parameter.comment*) = "" then
        []
      else
        [ String " // "; Description "" (*parameter.comment*) ]
    (defaultActual,
     [ String "("
       InlineParamName parameter.name
       String ": "
       TypeReference parameter.typ
       String ")" ]
     @ comment)


  | TCK.FunctionCallResult(fnName, returnType, _) ->
    // format:
    // Option<String>
    defaultActual, [ TypeReference returnType ]


  | TCK.RecordField(recordType, fieldName, fieldType, _) ->
    // format:
    // TODO: fetch the type and lookup the field definition as a comment
    let comment = []
    // ({ name : string; ... }) // some description
    // let comment =
    //   if fieldDef.description = "" then
    //     []
    //   else
    //     [ String " // "; Description fieldDef.description ]

    defaultActual,
    ([ String "({ "
       InlineFieldName fieldName
       String ": "
       TypeReference fieldType
       String "; ... })" ]
     @ comment)


  | TCK.DictKey(key, typ, _) ->
    // format:
    // ({ "name" : String; ... })
    defaultActual,
    ([ String "({ "
       InlineFieldName key
       String ": "
       TypeReference typ
       String "; ... })" ])


  | TCK.EnumField(enumType, caseName, fieldIndex, fieldCount, fieldType, _) ->
    // format:
    //   Ok (..., string, ...) // some description
    // TODO: extract description from the type definition later
    let prefix = if fieldIndex = 0 then [] else [ String "..., " ]
    let suffix = if fieldIndex = fieldCount - 1 then [] else [ String ", ..." ]
    let openParen = if fieldCount > 0 then [ String "(" ] else []
    let closeParen = if fieldCount > 0 then [ String ")" ] else []
    let display typ =
      [ ShortTypeName enumType; String "."; InlineFieldName caseName; String " " ]
      @ openParen
      @ prefix
      @ typ
      @ suffix
      @ closeParen


    (display [ TypeOfValue argument ]), (display [ TypeReference fieldType ])


  | TCK.DBSchemaType(dbName, expectedType, _) ->
    // format:
    // String
    defaultActual, [ TypeReference expectedType ]


  | TCK.DBQueryVariable(varName, expected, _) ->
    // format:
    // (varName : string) // some description
    defaultActual,
    ([ String "("
       InlineVarName varName

       String ": "
       TypeReference expected
       String ")" ])


  | TCK.ListIndex(index, typ, parent) -> defaultActual, [ TypeReference typ ]


  | TCK.TupleIndex(index, typ, parent) -> defaultActual, [ TypeReference typ ]

let contextVerb (context : TCK.Context) : string =
  match context with
  | TCK.FunctionCallParameter _ -> "passed"
  | TCK.FunctionCallResult _ -> "returned"
  | TCK.RecordField _ -> "passed"
  | TCK.DictKey _ -> "passed"
  | TCK.EnumField _ -> "passed"
  | TCK.DBSchemaType _ -> "passed"
  | TCK.DBQueryVariable _ -> "passed"
  | TCK.ListIndex _ -> "passed"
  | TCK.TupleIndex _ -> "passed"


let toSegments (e : Error) : ErrorOutput =
  match e with
  | TypeError(TCK.ValueNotExpectedType(argument, expected, context)) ->

    let summary =
      contextSummary context
      @ [ String " should be "; IndefiniteArticle; TypeReference expected ]

    let extraExplanation =
      [ String ". However, "
        IndefiniteArticle
        TypeOfValue argument
        String " ("
        InlineValue argument
        String ") was passed instead." ]

    let (actual, expected) = contextAsActualExpected argument context

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }

  | TypeError(TCK.MismatchedRecordFields(typeName, extra, missing, _)) ->
    let summary = [ TypeName typeName; String " has incorrect fields" ]

    let extraExplanation = []
    let actual =
      [ String "Extra fields were found: " ]
      @ (extra
         |> Set.toList
         |> List.map (fun name -> FieldName name)
         |> Tablecloth.List.intersperse (String ", "))
    let expected =
      [ String "Some fields were found: " ]
      @ (missing
         |> Set.toList
         |> List.map (fun name -> FieldName name)
         |> Tablecloth.List.intersperse (String ", "))

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }


  | TypeError(TCK.TypeDoesntExist(typeName, _)) ->
    // Perhaps this should be an internal error as this shouldn't be possible
    let summary = [ TypeName typeName; String " doesn't exist" ]

    let extraExplanation = []
    let actual = []
    let expected = []

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }

  | NameResolutionError({ errorType = NameResolution.NotFound } as e) ->
    let nameType =
      match e.nameType with
      | NameResolution.Function -> "function"
      | NameResolution.Type -> "type"
      | NameResolution.Constant -> "constant"

    let summary =
      [ String $"There is no {nameType} named "
        InlineVarName(String.concat "." e.names) ]

    let extraExplanation = []
    let actual = []
    let expected = []

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }

  | NameResolutionError({ errorType = NameResolution.MissingModuleName } as e) ->
    let suffix =
      if e.names = [] then
        []
      else
        [ String ": "; VarName(String.concat "." e.names) ]
    let summary = [ String "Missing module name" ] @ suffix

    let extraExplanation = []
    let actual = []
    let expected = []

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }

  | NameResolutionError({ errorType = NameResolution.InvalidPackageName } as e) ->
    let summary =
      [ String "Invalid package name "; InlineVarName(String.concat "." e.names) ]

    let extraExplanation = []
    let actual = []
    let expected = []

    { summary = summary
      extraExplanation = extraExplanation
      actual = actual
      expected = expected }

  | TypeError err ->

    { summary = [ String "VTTODO" ]
      extraExplanation = []
      actual = []
      expected = [] }

let toString (e : Error) : string =
  let s = toSegments e
  let explanation = ErrorSegment.toString (s.summary @ s.extraExplanation)
  let actual = ErrorSegment.toString s.actual
  let actual = if actual = "" then "" else "\nActual: " + actual
  let expected = ErrorSegment.toString s.expected
  let expected = if expected = "" then "" else "\nExpected: " + expected
  if actual = "" && expected = "" then
    explanation
  else
    $"{explanation}\n{expected}{actual}"
