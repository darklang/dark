/// Converts strings of F# into Dark. Used for testing.
module Parser

// refer to https://fsharp.github.io/fsharp-compiler-docs

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

let parse (input : string) : SynExpr =
  let file = "test.fs"
  let checker = FSharpChecker.Create()

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions = { FSharpParsingOptions.Default with SourceFiles = [| file |] }

  let results =
    checker.ParseFile(file, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  match results.ParseTree with
  | (ParsedInput.ImplFile (ParsedImplFileInput (_,
                                                _,
                                                _,
                                                _,
                                                _,
                                                [ SynModuleOrNamespace (_,
                                                                        _,
                                                                        _,
                                                                        [ SynModuleDecl.Expr (expr,
                                                                                              _) ],
                                                                        _,
                                                                        _,
                                                                        _,
                                                                        _,
                                                                        _) ],
                                                _,
                                                _,
                                                _))) -> expr
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parseTree", results.ParseTree; "input", input ]

// A placeholder is used to indicate what still needs to be filled
let placeholder = PT.EString(12345678UL, [ PT.StringText "PLACEHOLDER VALUE" ])

// This is a "Partial active pattern" that you can use as a Pattern to match a Placeholder value
let (|Placeholder|_|) (input : PT.Expr) =
  if input = placeholder then Some() else None

// CLEANUP - blanks here aren't allowed
let nameOrBlank (v : string) : string = if v = "___" then "" else v

let longIdentToList (li : LongIdent) : List<string> =
  li |> List.map (fun id -> id.idText)

let parseFn (fnName : string) : string * int =
  match fnName with
  | Regex "^([a-z][a-z0-9A-Z]*)_v(\d+)$" [ name; version ] -> name, (int version)
  | Regex "^([a-z][a-z0-9A-Z]*)$" [ name ] -> name, 0
  | _ ->
    Exception.raiseInternal
      "Bad format in one word function name"
      [ "fnName", fnName ]


let rec convertToExpr
  (userTypes : List<PT.UserTypeName * PT.UserType.Definition>)
  (ast : SynExpr)
  : PT.Expr =
  let c = convertToExpr userTypes

  let rec convertMatchPattern (pat : SynPat) : PT.MatchPattern =
    let id = gid ()
    match pat with
    | SynPat.Named (SynIdent (name, _), _, _, _) -> PT.MPVariable(id, name.idText)
    | SynPat.Wild _ -> PT.MPVariable(gid (), "_") // wildcard, not blank
    | SynPat.Const (SynConst.Int32 n, _) -> PT.MPInteger(id, n)
    | SynPat.Const (SynConst.Int64 n, _) -> PT.MPInteger(id, int64 n)
    | SynPat.Const (SynConst.UInt64 n, _) -> PT.MPInteger(id, int64 n)
    | SynPat.Const (SynConst.UserNum (n, "I"), _) -> PT.MPInteger(id, parseInt64 n)
    | SynPat.Const (SynConst.Char c, _) -> PT.MPCharacter(id, string c)
    | SynPat.Const (SynConst.Bool b, _) -> PT.MPBool(id, b)
    | SynPat.Const (SynConst.Unit, _) -> PT.MPUnit(id)
    | SynPat.Null _ ->
      Exception.raiseInternal "null pattern not supported, use `()`" [ "pat", pat ]
    | SynPat.Paren (pat, _) -> convertMatchPattern pat
    | SynPat.Const (SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.MPFloat(id, sign, whole, fraction)
    | SynPat.Const (SynConst.String (s, _, _), _) -> PT.MPString(id, s)
    | SynPat.LongIdent (SynLongIdent ([ constructorName ], _, _),
                        _,
                        _,
                        SynArgPats.Pats args,
                        _,
                        _) ->
      let args = List.map convertMatchPattern args
      PT.MPConstructor(id, constructorName.idText, args)
    | SynPat.Tuple (_isStruct, (first :: second :: theRest), _range) ->
      PT.MPTuple(
        id,
        convertMatchPattern first,
        convertMatchPattern second,
        List.map convertMatchPattern theRest
      )
    | _ -> Exception.raiseInternal "unhandled pattern" [ "pattern", pat ]

  let convertLambdaVar (var : SynSimplePat) : string =
    match var with
    | SynSimplePat.Id (name, _, _, _, _, _) -> nameOrBlank name.idText
    | _ -> Exception.raiseInternal "unsupported lambdaVar" [ "var", var ]

  // Add a pipetarget after creating it
  let cPlusPipeTarget (e : SynExpr) : PT.Expr =
    match c e with
    | PT.EFnCall (id, name, args) ->
      PT.EFnCall(id, name, PT.EPipeTarget(gid ()) :: args)
    | PT.EInfix (id, op, Placeholder, arg2) ->
      PT.EInfix(id, op, PT.EPipeTarget(gid ()), arg2)
    | PT.EInfix (id, op, arg1, Placeholder) ->
      PT.EInfix(id, op, PT.EPipeTarget(gid ()), arg1)
    | other -> other

  let ops =
    Map.ofList [ ("op_Addition", PT.ArithmeticPlus)
                 ("op_Subtraction", PT.ArithmeticMinus)
                 ("op_Multiply", PT.ArithmeticMultiply)
                 ("op_Division", PT.ArithmeticDivide)
                 ("op_Modulus", PT.ArithmeticModulo)
                 ("op_Concatenate", PT.ArithmeticPower)
                 ("op_GreaterThan", PT.ComparisonGreaterThan)
                 ("op_GreaterThanOrEqual", PT.ComparisonGreaterThanOrEqual)
                 ("op_LessThan", PT.ComparisonLessThan)
                 ("op_LessThanOrEqual", PT.ComparisonLessThanOrEqual)
                 ("op_EqualsEquals", PT.ComparisonEquals)
                 ("op_BangEquals", PT.ComparisonNotEquals)
                 ("op_PlusPlus", PT.StringConcat) ]

  let id = gid ()

  match ast with
  // constant values
  | SynExpr.Null _ ->
    Exception.raiseInternal "null not supported, use `()`" [ "ast", ast ]
  | SynExpr.Const (SynConst.Unit _, _) -> PT.EUnit id
  | SynExpr.Const (SynConst.Int32 n, _) -> PT.EInteger(id, n)
  | SynExpr.Const (SynConst.Int64 n, _) -> PT.EInteger(id, int64 n)
  | SynExpr.Const (SynConst.UInt64 n, _) -> PT.EInteger(id, int64 n)
  | SynExpr.Const (SynConst.UserNum (n, "I"), _) -> PT.EInteger(id, parseInt64 n)
  | SynExpr.Const (SynConst.Char c, _) -> PT.ECharacter(id, string c)
  | SynExpr.Const (SynConst.Bool b, _) -> PT.EBool(id, b)
  | SynExpr.Const (SynConst.Double d, _) ->
    let sign, whole, fraction = readFloat d
    PT.EFloat(id, sign, whole, fraction)
  | SynExpr.Const (SynConst.String (s, _, _), _) ->
    PT.EString(id, [ PT.StringText s ])
  | SynExpr.InterpolatedString (parts, _, _) ->
    let parts =
      parts
      |> List.map (function
        | SynInterpolatedStringPart.String (s, _) -> PT.StringText s
        | SynInterpolatedStringPart.FillExpr (e, _) -> PT.StringInterpolation(c e))
    PT.EString(id, parts)

  // simple identifiers like `==`
  | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
    Map.containsKey ident.idText ops
    ->
    let op =
      Map.get ident.idText ops
      |> Exception.unwrapOptionInternal
           "can't find operation"
           [ "name", ident.idText ]
    PT.EInfix(id, PT.InfixFnCall op, placeholder, placeholder)

  | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
    List.contains ident.idText [ "op_BooleanAnd"; "op_BooleanOr" ]
    ->
    let op =
      match ident.idText with
      | "op_BooleanAnd" -> PT.BinOpAnd
      | "op_BooleanOr" -> PT.BinOpOr
      | _ -> Exception.raiseInternal "unhandled operation" [ "name", ident.idText ]

    PT.EInfix(id, PT.BinOp op, placeholder, placeholder)

  | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
    ident.idText = "op_UnaryNegation"
    ->
    let name = PT.FQFnName.stdlibFqName "Int" "negate" 0
    PT.EFnCall(id, name, [])

  | SynExpr.Ident ident when Set.contains ident.idText PT.FQFnName.oneWordFunctions ->
    let name, version = parseFn ident.idText
    PT.EFnCall(id, PT.FQFnName.stdlibFqName "" name version, [])

  | SynExpr.Ident ident when ident.idText = "Nothing" ->
    PT.EConstructor(id, None, "Nothing", [])

  | SynExpr.Ident name ->
    let matchingEnumCases =
      userTypes
      |> List.choose (fun (typeName, typeDef) ->
        match typeDef with
        | PT.UserType.Enum (firstCase, additionalCases) ->
          firstCase :: additionalCases
          |> List.tryFind (fun enumCase -> enumCase.name = name.idText)
          |> Option.map (fun _ -> typeName)

        | PT.UserType.Record _ -> None)

    match matchingEnumCases with
    | [] -> PT.EVariable(id, name.idText)
    | [ userFnName ] ->
      let fields =
        // The fields of an enum ctor are filled in later, since
        // the F# parser encodes callers with multiple args
        // as apps wrapping other apps.
        //
        // In any case, the fields are filled in shortly - search through
        // this file for other cases of "EConstructor" to locate such.
        []

      PT.EConstructor(id, Some userFnName, name.idText, fields)
    | _ ->
      Exception.raiseInternal
        "There are more than 1 values that match this name, so the parser isn't sure which one to choose"
        [ "name", name.idText ]

  // lists and arrays
  | SynExpr.ArrayOrList (_, exprs, _) -> PT.EList(id, exprs |> List.map c)

  // A literal list is sometimes made up of nested Sequentials
  | SynExpr.ArrayOrListComputed (_, (SynExpr.Sequential _ as seq), _) ->
    let rec seqAsList expr : List<SynExpr> =
      match expr with
      | SynExpr.Sequential (_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
      | _ -> [ expr ]
    PT.EList(id, seq |> seqAsList |> List.map c)

  | SynExpr.ArrayOrListComputed (_, SynExpr.Tuple (_, exprs, _, _), _) ->
    PT.EList(id, exprs |> List.map c)

  | SynExpr.ArrayOrListComputed (_, expr, _) -> PT.EList(id, [ c expr ])

  // tuples
  | SynExpr.Tuple (_, first :: second :: rest, _, _) ->
    PT.ETuple(id, c first, c second, List.map c rest)

  // Long Identifiers like DateTime.now, represented in the form of ["DateTime"; "now"]
  // (LongIdent = Ident list)
  | SynExpr.LongIdent (_, SynLongIdent ([ modName; fnName ], _, _), _, _) when
    System.Char.IsUpper(modName.idText[0])
    ->
    let module_ = modName.idText
    let name, version = parseFn fnName.idText
    PT.EFnCall(gid (), PT.FQFnName.stdlibFqName module_ name version, [])


  // Preliminary support for package manager functions
  | SynExpr.LongIdent (_,
                       SynLongIdent ([ owner; package; modName; fnName ], _, _),
                       _,
                       _) when
    owner.idText = "Test" && package.idText = "Test" && modName.idText = "Test"
    ->
    PT.EFnCall(
      gid (),
      PT.FQFnName.packageFqName "test" "test" "Test" fnName.idText 0,
      []
    )

  | SynExpr.LongIdent (_, SynLongIdent ([ var; f1; f2; f3 ], _, _), _, _) ->
    let obj1 =
      PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank f1.idText)
    let obj2 = PT.EFieldAccess(id, obj1, nameOrBlank f2.idText)
    PT.EFieldAccess(id, obj2, nameOrBlank f3.idText)

  | SynExpr.LongIdent (_, SynLongIdent ([ var; field1; field2 ], _, _), _, _) ->
    let obj1 =
      PT.EFieldAccess(
        id,
        PT.EVariable(gid (), var.idText),
        nameOrBlank field1.idText
      )
    PT.EFieldAccess(id, obj1, nameOrBlank field2.idText)

  | SynExpr.LongIdent (_, SynLongIdent ([ var; field ], _, _), _, _) ->
    PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank field.idText)

  | SynExpr.DotGet (expr, _, SynLongIdent ([ field ], _, _), _) ->
    PT.EFieldAccess(id, c expr, nameOrBlank field.idText)

  | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (outerVars, _), body, _, _, _) ->
    let rec extractVarsAndBody expr =
      match expr with
      // The 2nd param indicates this was part of a lambda
      | SynExpr.Lambda (_, true, SynSimplePats.SimplePats (vars, _), body, _, _, _) ->
        let nestedVars, body = extractVarsAndBody body
        vars @ nestedVars, body
      // The 2nd param indicates this was not nested
      | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (vars, _), body, _, _, _) ->
        vars, body
      | SynExpr.Lambda _ ->
        Exception.raiseInternal "TODO: other types of lambda" [ "expr", expr ]
      | _ -> [], expr

    let nestedVars, body = extractVarsAndBody body
    let vars =
      (outerVars @ nestedVars)
      |> List.map convertLambdaVar
      |> (List.map (fun name -> (gid (), name)))
    PT.ELambda(id, vars, c body)

  // if/then expressions
  | SynExpr.IfThenElse (cond, thenExpr, Some elseExpr, _, _, _, _) ->
    PT.EIf(id, c cond, c thenExpr, c elseExpr)

  // handle `let` bindings
  | SynExpr.LetOrUse (_,
                      _,
                      [ SynBinding (_, _, _, _, _, _, _, pat, _, rhs, _, _, _) ],
                      body,
                      _,
                      _) ->

    let rec mapPat (pat : SynPat) : PT.LetPattern =
      match pat with
      | SynPat.Paren (subPat, _) -> mapPat subPat
      | SynPat.Wild (_) -> PT.LPVariable(gid (), "_")
      | SynPat.Named (SynIdent (name, _), _, _, _) ->
        PT.LPVariable(gid (), name.idText)
      | _ ->
        Exception.raiseInternal
          "Unsupported let or use expr pat type"
          [ "ast", ast; "pat", pat ]

    PT.ELet(id, mapPat pat, c rhs, c body)

  | SynExpr.Match (_, cond, clauses, _, _) ->
    let convertClause
      (SynMatchClause (pat, _, expr, _, _, _) : SynMatchClause)
      : PT.MatchPattern * PT.Expr =
      (convertMatchPattern pat, c expr)
    PT.EMatch(id, c cond, List.map convertClause clauses)

  | SynExpr.Record (_, _, fields, _) ->
    let fields =
      fields
      |> List.map (fun field ->
        match field with
        | SynExprRecordField ((SynLongIdent ([ name ], _, _), _), _, Some expr, _) ->
          (nameOrBlank name.idText, c expr)
        | f -> Exception.raiseInternal "Not an expected field" [ "field", f ])

    let typeName = None // TODO: determine the appropriate typeName based on the fields and types available

    PT.ERecord(id, typeName, fields)

  | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap

  | SynExpr.Do (expr, _) -> c expr // just unwrap


  // handle pipes (|>)
  // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
  // op_PipeRight, and the inner app has two arguments. Those arguments might
  // also be pipes
  | SynExpr.App (_, _, SynExpr.Ident pipe, SynExpr.App (_, _, nestedPipes, arg, _), _)
  | SynExpr.App (_,
                 _,
                 SynExpr.LongIdent (_, SynLongIdent ([ pipe ], _, _), _, _),
                 SynExpr.App (_, _, nestedPipes, arg, _),
                 _) when pipe.idText = "op_PipeRight" ->
    match c nestedPipes with
    | PT.EPipe (id, arg1, Placeholder, []) ->
      // when we just built the lowest, the second one goes here
      PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
    | PT.EPipe (id, arg1, arg2, rest) ->
      PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
    // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
    | other ->
      // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      // the very bottom on the pipe chain, this is the first and second expressions
      PT.EPipe(id, other, cPlusPipeTarget arg, [])

  | SynExpr.App (_, _, SynExpr.Ident pipe, expr, _)
  | SynExpr.App (_,
                 _,
                 SynExpr.LongIdent (_, SynLongIdent ([ pipe ], _, _), _, _),
                 expr,
                 _) when pipe.idText = "op_PipeRight" ->
    // the very bottom on the pipe chain, this is just the first expression
    PT.EPipe(id, c expr, placeholder, [])




  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when
    List.contains name.idText [ "Ok"; "Nothing"; "Just"; "Error" ]
    ->
    PT.EConstructor(id, None, name.idText, [ c arg ])

  // Feature flag now or else it'll get recognized as a var
  | SynExpr.App (_,
                 _,
                 SynExpr.Ident name,
                 SynExpr.Const (SynConst.String (label, _, _), _),
                 _) when name.idText = "flag" ->
    PT.EFeatureFlag(gid (), label, placeholder, placeholder, placeholder)

  // Callers with multiple args are encoded as apps wrapping other apps.
  | SynExpr.App (_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
    match c funcExpr with
    | PT.EFnCall (id, name, args) -> PT.EFnCall(id, name, args @ [ c arg ])
    | PT.EInfix (id, op, Placeholder, arg2) -> PT.EInfix(id, op, c arg, arg2)
    | PT.EInfix (id, op, arg1, Placeholder) -> PT.EInfix(id, op, arg1, c arg)
    // Fill in the feature flag fields (back to front)
    | PT.EFeatureFlag (id, label, Placeholder, oldexpr, newexpr) ->
      PT.EFeatureFlag(id, label, c arg, oldexpr, newexpr)
    | PT.EFeatureFlag (id, label, condexpr, Placeholder, newexpr) ->
      PT.EFeatureFlag(id, label, condexpr, c arg, newexpr)
    | PT.EFeatureFlag (id, label, condexpr, oldexpr, Placeholder) ->
      PT.EFeatureFlag(id, label, condexpr, oldexpr, c arg)
    // A pipe with one entry
    | PT.EPipe (id, arg1, Placeholder, []) ->
      PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
    // A pipe with more than one entry
    | PT.EPipe (id, arg1, arg2, rest) ->
      PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
    | PT.EVariable (id, name) ->
      if Set.contains name PT.FQFnName.oneWordFunctions then
        let (name, version) = parseFn name
        PT.EFnCall(id, PT.FQFnName.stdlibFqName "" name version, [ c arg ])
      else
        PT.EFnCall(id, PT.FQFnName.User name, [ c arg ])

    // Enums
    | PT.EConstructor (id, typeName, caseName, _fields) ->
      let fields =
        match c arg with
        | PT.ETuple (_, first, second, theRest) -> first :: second :: theRest
        | other -> [ other ]

      PT.EConstructor(id, typeName, caseName, fields)

    | e ->
      Exception.raiseInternal
        "Unsupported expression in app"
        [ "fnCall expr", funcExpr
          "converted specific fncall exp", e
          "argument", arg ]

  | SynExpr.FromParseError _ as expr ->
    Exception.raiseInternal "There was a parser error parsing" [ "expr", expr ]
  | expr ->
    Exception.raiseInternal "Unsupported expression" [ "ast", ast; "expr", expr ]

type Test = { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

let convertToTest userTypes (ast : SynExpr) : Test =
  let convert (x : SynExpr) : PT.Expr = convertToExpr userTypes x

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_,
                              _,
                              SynExpr.LongIdent (_,
                                                 SynLongIdent ([ ident ], _, _),
                                                 _,
                                                 _),
                              actual,
                              _),
                 expected,
                 range) when ident.idText = "op_Equality" ->
    // Exception.raiseInternal $"whole thing: {actual}"
    { name = "test"
      lineNumber = range.Start.Line
      actual = convert actual
      expected = convert expected }
  | _ -> Exception.raiseInternal "Test case not in format `x = y`" [ "ast", ast ]

type Module =
  { types : List<PT.UserType.T>
    dbs : List<PT.DB.T>
    fns : List<PT.UserFunction.T>
    packageFns : List<PT.Package.Fn>
    modules : List<string * Module>
    tests : List<Test> }

let emptyModule =
  { types = []; dbs = []; fns = []; modules = []; tests = []; packageFns = [] }

let parseTestFile (filename : string) : Module =
  let checker = FSharpChecker.Create()
  let input = System.IO.File.ReadAllText filename

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions =
    { FSharpParsingOptions.Default with SourceFiles = [| filename |] }

  let fsharpFilename = System.IO.Path.GetFileNameWithoutExtension filename + ".fs"

  let results =
    checker.ParseFile(fsharpFilename, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  // TODO: support user-defined types that aren't the 0th
  // version of the type
  let matchingUserTypes
    (userTypes : List<PT.UserTypeName * PT.UserType.Definition>)
    (nameOfType : string)
    =
    userTypes
    |> List.map (fun (typeName, _def) -> typeName)
    |> List.filter (fun typeName -> typeName.typ = nameOfType)

  let rec convertType
    (userTypes : List<PT.UserTypeName * PT.UserType.Definition>)
    (typ : SynType)
    : PT.DType =
    let c = convertType userTypes
    match typ with
    | SynType.App (SynType.LongIdent (SynLongIdent ([ ident ], _, _)),
                   _,
                   args,
                   _,
                   _,
                   _,
                   _) ->
      match ident.idText, matchingUserTypes userTypes ident.idText, args with
      | "List", _, [ arg ] -> PT.TList(c arg)
      | "Option", _, [ arg ] -> PT.TOption(c arg)
      | _, [ typeName ], _ -> PT.TUserType typeName
      | _ ->
        Exception.raiseInternal
          $"Unsupported type (outer)"
          [ "type", typ; "name", ident.idText; "range", ident; "id", ident.idRange ]

    | SynType.Paren (t, _) -> c t
    | SynType.Fun (arg, ret, _, _) -> PT.TFn([ c arg ], c ret)
    | SynType.Var (SynTypar (id, _, _), _) -> PT.TVariable(id.idText)
    | SynType.LongIdent (SynLongIdent ([ ident ], _, _)) ->

      match ident.idText with
      | "bool" -> PT.TBool
      | "int" -> PT.TInt
      | "string" -> PT.TStr
      | "char" -> PT.TChar
      | "float" -> PT.TFloat
      | "DateTime" -> PT.TDateTime
      | "UUID" -> PT.TUuid
      | "unit" -> PT.TUnit
      | "Password" -> PT.TPassword
      | _ ->
        match matchingUserTypes userTypes ident.idText with
        | [ matched ] -> PT.TUserType matched
        | _ ->
          Exception.raiseInternal
            $"Unsupported type"
            [ "name", ident.idText; "type", typ; "range", ident.idRange ]
    | _ -> Exception.raiseInternal $"Unsupported type" [ "type", typ ]

  let rec parseArgPat userTypes (pat : SynPat) : PT.UserFunction.Parameter =
    match pat with
    | SynPat.Paren (pat, _) -> parseArgPat userTypes pat
    | SynPat.Const (SynConst.Unit, _) ->
      { id = gid (); name = "unit"; typ = PT.TUnit; description = "" }
    | SynPat.Typed (SynPat.Named (SynIdent (id, _), _, _, _), typ, _) ->
      { id = gid ()
        name = id.idText
        typ = convertType userTypes typ
        description = "" }
    | _ -> Exception.raiseInternal "Unsupported argPat" [ "pat", pat ]

  let parseSignature
    userTypes
    (pat : SynPat)
    : string * List<PT.UserFunction.Parameter> =
    match pat with
    | SynPat.LongIdent (SynLongIdent ([ name ], _, _), _, _, argPats, _, _) ->
      let parameters =
        match argPats with
        | SynArgPats.Pats pats -> List.map (parseArgPat userTypes) pats
        | SynArgPats.NamePatPairs _ ->
          Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]
      (name.idText, parameters)
    | _ -> Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]

  let parseBinding userTypes (binding : SynBinding) : PT.UserFunction.T =
    match binding with
    // CLEANUP returnInfo
    | SynBinding (_, _, _, _, _, _, _, pat, returnInfo, expr, _, _, _) as x ->
      let (name, parameters) = parseSignature userTypes pat
      { tlid = gid ()
        name = name
        parameters = parameters
        returnType = PT.TVariable "a"
        description = ""
        infix = false
        body = convertToExpr userTypes expr }

  let parsePackageFn
    ((p1, p2, p3) : string * string * string)
    (binding : SynBinding)
    : PT.Package.Fn =
    let userTypes = [] // eventually, we'll likely need custom types supported here
    let userFn = parseBinding userTypes binding
    { name =
        { owner = p1
          package = p2
          module_ = p3
          function_ = userFn.name
          version = 0 }
      parameters =
        userFn.parameters
        |> List.map (fun (p : PT.UserFunction.Parameter) ->
          { name = p.name; typ = p.typ; description = "" })
      returnType = userFn.returnType
      description = userFn.description
      deprecated = false
      author = ""
      tlid = userFn.tlid
      body = userFn.body }


  let parseUserRecordTypeField
    userTypes
    (field : SynField)
    : PT.UserType.RecordField =
    match field with
    | SynField (_, _, Some id, typ, _, _, _, _, _) ->
      { id = gid (); name = id.idText; typ = convertType userTypes typ }
    | _ -> Exception.raiseInternal $"Unsupported field" [ "field", field ]

  let parseUserEnumTypeField userTypes (typ : SynField) : PT.UserType.EnumField =
    match typ with
    | SynField (_, _, fieldName, typ, _, _, _, _, _) ->
      { id = gid ()
        typ = convertType userTypes typ
        label = fieldName |> Option.map (fun id -> id.idText) }

  let parseUserEnumTypeCase userTypes (case : SynUnionCase) : PT.UserType.EnumCase =
    match case with
    | SynUnionCase (_, SynIdent (id, _), typ, _, _, _, _) ->
      match typ with
      | SynUnionCaseKind.Fields fields ->
        { id = gid ()
          name = id.idText
          fields = List.map (parseUserEnumTypeField userTypes) fields }
      | _ -> Exception.raiseInternal $"Unsupported enum case" [ "case", case ]

  let parseType userTypes (typeDef : SynTypeDefn) : PT.UserType.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->

      match fields with
      | [] ->
        Exception.raiseInternal
          $"Unsupported record type with no fields"
          [ "typeDef", typeDef ]
      | firstField :: additionalFields ->
        let parseField = parseUserRecordTypeField userTypes


        { tlid = gid ()
          name = { typ = id.idText; version = 0 }
          definition =
            PT.UserType.Record(
              parseField firstField,
              List.map parseField additionalFields
            ) }

    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->
      let parseCase = parseUserEnumTypeCase userTypes
      { tlid = gid ()
        name = { typ = id.idText; version = 0 }
        definition =
          let firstCase, additionalCases =
            match cases with
            | [] ->
              Exception.raiseInternal
                $"Can't parse enum without any cases"
                [ "typeDef", typeDef ]
            | firstCase :: additionalCases -> firstCase, additionalCases

          PT.UserType.Enum(parseCase firstCase, List.map parseCase additionalCases) }
    | _ ->
      Exception.raiseInternal $"Unsupported type definition" [ "typeDef", typeDef ]

  let parseDBSchemaField userTypes (field : SynField) : PT.DB.Col =
    match field with
    | SynField (_, _, Some id, typ, _, _, _, _, _) ->
      { name = Some(id.idText) // CLEANUP
        nameID = gid ()
        typ = Some(convertType userTypes typ)
        typeID = gid () }
    | _ -> Exception.raiseInternal $"Unsupported DB schema field" [ "field", field ]



  let parseDB userTypes (typeDef : SynTypeDefn) : PT.DB.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   members,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = id.idText
        nameID = gid ()
        version = 0
        cols = List.map (parseDBSchemaField userTypes) fields }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]

  let parseTypeDecl
    userTypes
    (typeDef : SynTypeDefn)
    : List<PT.DB.T> * List<PT.UserType.T> =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
      let attrs = attrs |> List.map (fun attr -> attr.Attributes) |> List.concat
      let isDB =
        attrs
        |> List.exists (fun attr ->
          longIdentToList attr.TypeName.LongIdent = [ "DB" ])
      if isDB then
        [ parseDB userTypes typeDef ], []
      else
        [], [ parseType userTypes typeDef ]

  let getPackage (attrs : SynAttributes) : Option<string * string * string> =
    attrs
    |> List.map (fun attr -> attr.Attributes)
    |> List.concat
    |> List.filterMap (fun (attr : SynAttribute) ->
      if longIdentToList attr.TypeName.LongIdent = [ "Package" ] then
        match attr.ArgExpr with
        | SynExpr.Paren (SynExpr.Tuple (_,
                                        [ SynExpr.Const (SynConst.String (p1, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p2, _, _),
                                                         _)
                                          SynExpr.Const (SynConst.String (p3, _, _),
                                                         _) ],
                                        _,
                                        _),
                         _,
                         _,
                         _) -> Some(p1, p2, p3)
        | _ -> None
      else
        None)
    |> List.tryHead

  let rec parseModule
    (parent : Module)
    (attrs : SynAttributes)
    (decls : List<SynModuleDecl>)
    : Module =
    let package = getPackage attrs
    List.fold
      { types = parent.types
        fns = parent.fns
        packageFns = parent.packageFns
        dbs = parent.dbs
        modules = []
        tests = [] }
      (fun m decl ->
        let availableTypes =
          (m.types @ parent.types) |> List.map (fun t -> t.name, t.definition)

        match decl with
        | SynModuleDecl.Let (_, bindings, _) ->
          match package with
          | Some package ->
            { m with
                packageFns =
                  m.packageFns @ List.map (parsePackageFn package) bindings }
          | None ->
            { m with fns = m.fns @ List.map (parseBinding availableTypes) bindings }

        | SynModuleDecl.Types (defns, _) ->
          let (dbs, types) =
            List.map (parseTypeDecl availableTypes) defns |> List.unzip
          { m with
              types = m.types @ List.concat types
              dbs = m.dbs @ List.concat dbs }

        | SynModuleDecl.Expr (SynExpr.Do (expr, _), _) ->
          { m with tests = m.tests @ [ convertToTest availableTypes expr ] }

        | SynModuleDecl.Expr (expr, _) ->
          { m with tests = m.tests @ [ convertToTest availableTypes expr ] }

        | SynModuleDecl.NestedModule (SynComponentInfo (attrs,
                                                        _,
                                                        _,
                                                        [ name ],
                                                        _,
                                                        _,
                                                        _,
                                                        _),
                                      _,
                                      decls,
                                      _,
                                      _,
                                      _) ->
          let nested = parseModule m attrs decls
          { m with modules = m.modules @ [ (name.idText, nested) ] }
        | _ -> Exception.raiseInternal $"Unsupported declaration" [ "decl", decl ])
      decls

  match results.ParseTree with
  | (ParsedInput.ImplFile (ParsedImplFileInput (_,
                                                _,
                                                _,
                                                _,
                                                _,
                                                [ SynModuleOrNamespace ([ _id ],
                                                                        _,
                                                                        _,
                                                                        decls,
                                                                        _,
                                                                        attrs,
                                                                        _,
                                                                        _,
                                                                        _) ],
                                                _,
                                                _,
                                                _))) ->
    parseModule emptyModule attrs decls
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree - ensure that input is a single expression, perhaps by wrapping the existing code in parens"
      [ "parseTree", results.ParseTree; "input", input; "filename", filename ]


let parsePTExprWithTypes userTypes (code : string) : PT.Expr =
  code |> parse |> convertToExpr userTypes

let parseRTExprWithTypes userTypes (code : string) : RT.Expr =
  parsePTExprWithTypes userTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parsePTExpr = parsePTExprWithTypes []

let parseRTExpr = parseRTExprWithTypes []
