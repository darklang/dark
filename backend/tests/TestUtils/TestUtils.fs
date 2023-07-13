module TestUtils.TestUtils

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Exe = LibExecution.Execution
module S = RTShortcuts

let testOwner : Lazy<Task<UserID>> = lazy (Account.createUser ())

let nameToTestDomain (name : string) : string =
  let name =
    name
    |> String.toLowercase
    // replace invalid chars with a single hyphen
    |> FsRegEx.replace "[^-a-z0-9]+" "-"
    |> FsRegEx.replace "[-_]+" "-"
    |> String.take 50
  let suffix = randomString 5 |> String.toLowercase
  $"{name}-{suffix}"
  |> FsRegEx.replace "[-_]+" "-"
  |> fun s -> $"{s}.dlio.localhost"

let initializeCanvasForOwner
  (ownerID : UserID)
  (name : string)
  : Task<CanvasID * string> =
  task {
    let domain = nameToTestDomain name
    let! canvasID = Canvas.create ownerID domain
    return (canvasID, domain)
  }

let initializeTestCanvas' (name : string) : Task<CanvasID * string> =
  task {
    let! owner = testOwner.Force()
    return! initializeCanvasForOwner owner name
  }

let initializeTestCanvas (name : string) : Task<CanvasID> =
  task {
    let! (canvasID, domain) = initializeTestCanvas' name
    return canvasID
  }


let testHttpRouteHandler
  (route : string)
  (method : string)
  (ast : PT.Expr)
  : PT.Handler.T =
  { tlid = gid (); ast = ast; spec = PT.Handler.HTTP(route, method) }

let testCron
  (name : string)
  (interval : PT.Handler.CronInterval)
  (ast : PT.Expr)
  : PT.Handler.T =
  { tlid = gid (); ast = ast; spec = PT.Handler.Cron(name, interval) }

let testWorker (name : string) (ast : PT.Expr) : PT.Handler.T =
  { tlid = gid (); ast = ast; spec = PT.Handler.Worker name }

let testUserFn
  (name : string)
  (typeParams : List<string>)
  (parameters : string list)
  (returnType : PT.TypeReference)
  (body : PT.Expr)
  : PT.UserFunction.T =
  { tlid = gid ()
    body = body
    description = ""
    name = PT.FnName.userProgram [] name 0
    typeParams = typeParams
    deprecated = PT.NotDeprecated
    parameters =
      List.map
        (fun p -> { name = p; typ = PT.TVariable "b"; description = "test" })
        parameters
    returnType = returnType }

let testUserRecordType
  (name : PT.TypeName.UserProgram)
  (firstField : string * PT.TypeReference)
  (additionalFields : List<string * PT.TypeReference>)
  : PT.UserType.T =
  let mapField (name, typ) : PT.TypeDeclaration.RecordField =
    { name = name; typ = typ; description = "" }

  { tlid = gid ()
    name = name
    description = ""
    deprecated = PT.NotDeprecated
    declaration =
      { typeParams = []
        definition =
          PT.TypeDeclaration.Record(
            mapField firstField,
            List.map mapField additionalFields
          ) } }



let testDB (name : string) (typ : PT.TypeReference) : PT.DB.T =
  { tlid = gid (); name = name; typ = typ; version = 0 }

let builtIns : RT.BuiltIns =
  let (fns, types) =
    LibExecution.StdLib.combine
      [ LibTest.contents
        LibRealExecution.RealExecution.builtins
        StdLibCli.StdLib.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name) }

let executionStateFor
  (canvasID : CanvasID)
  (internalFnsAllowed : bool)
  (allowLocalHttpAccess : bool)
  (dbs : Map<string, RT.DB.T>)
  (userTypes : Map<RT.TypeName.UserProgram, RT.UserType.T>)
  (userFunctions : Map<RT.FnName.UserProgram, RT.UserFunction.T>)
  : Task<RT.ExecutionState> =
  task {
    let! domains = Canvas.domainsForCanvasID canvasID
    let config : RT.Config =
      // Short timeout so that tests using the timeout complete quickly
      { allowLocalHttpAccess = allowLocalHttpAccess; httpclientTimeoutInMs = 5000 }

    let program : RT.Program =
      { canvasID = canvasID
        internalFnsAllowed = internalFnsAllowed
        fns = userFunctions
        types = userTypes
        dbs = dbs
        secrets = [] }

    let testContext : RT.TestContext =
      { sideEffectCount = 0
        exceptionReports = []
        expectedExceptionCount = 0
        postTestExecutionHook =
          fun tc _ ->
            // In an effort to find errors in the test suite, we track exceptions
            // that we report in the runtime and check for them after the test
            // completes.  There are a lot of places where exceptions are allowed,
            // possibly too many to annotate, so we assume that errors are intended
            // to be reported anytime the result is a DError.
            let exceptionCountMatches =
              tc.exceptionReports.Length = tc.expectedExceptionCount

            if not exceptionCountMatches then
              List.iter
                (fun (msg, stackTrace, metadata) ->
                  print
                    $"An error was reported in the runtime:  \n  {msg}\n{stackTrace}\n  {metadata}\n\n")
                tc.exceptionReports
              Exception.raiseInternal
                $"UNEXPECTED EXCEPTION COUNT in test {domains}"
                [ "expectedExceptionCount", tc.expectedExceptionCount
                  "actualExceptionCount", tc.exceptionReports.Length
                  "reports", tc.exceptionReports ] }

    // Typically, exceptions thrown in tests have surprised us. Take these errors and
    // catch them much closer to where they happen (usually in the function
    // definition)
    let rec exceptionReporter : RT.ExceptionReporter =
      fun (state : RT.ExecutionState) metadata (exn : exn) ->
        let message = exn.Message
        let stackTrace = exn.StackTrace
        let metadata = Exception.toMetadata exn @ metadata
        let inner = exn.InnerException
        if not (isNull inner) then (exceptionReporter state [] inner) else ()
        state.test.exceptionReports <-
          (message, stackTrace, metadata) :: state.test.exceptionReports

    // For now, lets not track notifications, as often our tests explicitly trigger
    // things that notify, while Exceptions have historically been unexpected errors
    // in the tests and so are worth watching out for.
    let notifier : RT.Notifier = fun _state _msg _tags -> ()
    let state =
      Exe.createState
        builtIns
        LibBackend.PackageManager.packageManager
        (Exe.noTracing RT.Real)
        exceptionReporter
        notifier
        (id 7)
        program
        config
    let state = { state with test = testContext }
    return state
  }

/// Saves and reloads the canvas for the Toplevels
let canvasForTLs (canvasID : CanvasID) (tls : List<PT.Toplevel.T>) : Task<Canvas.T> =
  task {
    let descs = tls |> List.map (fun tl -> (tl, LibBackend.Serialize.NotDeleted))
    do! Canvas.saveTLIDs canvasID descs
    return! Canvas.loadAll canvasID
  }



let testMany (name : string) (fn : 'a -> 'b) (values : List<'a * 'b>) =
  testList
    name
    (List.mapi
      (fun i (input, expected) ->
        test $"{name}[{i}]: ({input}) -> {expected}" {
          Expect.equal (fn input) expected ""
        })
      values)

let testMany2 (name : string) (fn : 'a -> 'b -> 'c) (values : List<'a * 'b * 'c>) =
  testList
    name
    (List.mapi
      (fun i (input1, input2, expected) ->
        test $"{name}[{i}]: ({input1}, {input2}) -> {expected}" {
          Expect.equal (fn input1 input2) expected ""
        })
      values)

let testManyTask (name : string) (fn : 'a -> Task<'b>) (values : List<'a * 'b>) =
  testList
    name
    (List.mapi
      (fun i (input, expected) ->
        testTask $"{name} - {i}" {
          let! result = fn input
          Expect.equal result expected ""
        })
      values)

let testMany2Task
  (name : string)
  (fn : 'a -> 'b -> Task<'c>)
  (values : List<'a * 'b * 'c>)
  =
  testList
    name
    (List.mapi
      (fun i (input1, input2, expected) ->
        testTask $"{name}[{i}]: ({input1}, {input2}) -> {expected}" {
          let! result = fn input1 input2
          Expect.equal result expected ""
        })
      values)



// Allow reusing property-test definitions with test cases found by fuzzing
let testListUsingProperty
  (name : string)
  (prop : 'a -> bool)
  (list : (string * 'a) list)
  =
  testList
    name
    (List.map
      (fun (doc, testCase) ->
        let doc = if doc = "" then testCase.ToString() else doc
        testTask $"{name} {doc}" { return (Expect.isTrue (prop testCase) "") })
      list)

let testListUsingPropertyAsync
  (name : string)
  (prop : 'a -> Task<bool>)
  (list : (string * 'a) list)
  =
  testList
    name
    (List.map
      (fun (doc, testCase) ->
        let doc = if doc = "" then testCase.ToString() else doc
        testTask $"{name} {doc}" {
          let! result = prop testCase
          return (Expect.isTrue result "")
        })
      list)

// Remove random things like IDs to make the tests stable
let normalizeDvalResult (dv : RT.Dval) : RT.Dval =
  match dv with
  | RT.DError(_, str) -> RT.DError(RT.SourceNone, str)
  | RT.DIncomplete _ -> RT.DIncomplete(RT.SourceNone)
  | dv -> dv

open LibExecution.RuntimeTypes

let rec debugDval (v : Dval) : string =
  match v with
  | DString s ->
    $"DString '{s}'(len {s.Length}, {System.BitConverter.ToString(UTF8.toBytes s)})"
  | DDateTime d ->
    $"DDateTime '{DarkDateTime.toIsoString d}': (millies {d.InUtc().Millisecond})"
  | DRecord(tn, o) ->
    let typeStr = TypeName.toString tn
    o
    |> Map.toList
    |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
    |> String.concat ",\n  "
    |> fun contents -> $"DRecord {typeStr} {{\n  {contents}}}"
  | DDict obj ->
    obj
    |> Map.toList
    |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
    |> String.concat ",\n  "
    |> fun contents -> $"DDict {{\n  {contents}}}"
  | DBytes b ->
    b
    |> Array.toList
    |> List.map string
    |> String.concat ", "
    // TODO: when I try to prepend this with "DBytes ",
    // the console output is "DB[|...|]", and the "ytes " are cut off.
    // Why?
    |> fun s -> $"[|{s}|]"

  | _ -> v.ToString()

module Expect =
  // Checks if the value (and all its contents) is in its desired
  // representation (in the event that there are multiple ways to represent
  // it). Think of this as a general form of string normalization.
  let rec isCanonical (dv : Dval) : bool =
    let check = isCanonical

    match dv with
    | DDateTime _
    | DIncomplete _
    | DInt _
    | DDateTime _
    | DBool _
    | DFloat _
    | DUnit
    | DPassword _
    | DFnVal _
    | DError _
    | DDB _
    | DUuid _
    | DBytes _
    | DFloat _ -> true
    | DList vs -> List.all check vs
    | DTuple(first, second, rest) -> List.all check ([ first; second ] @ rest)
    | DDict vs -> vs |> Map.values |> List.all check
    | DRecord(_, vs) -> vs |> Map.values |> List.all check
    | DString str -> str.IsNormalized()
    | DChar str -> str.IsNormalized() && String.lengthInEgcs str = 1
    | DEnum(_typeName, _caseName, fields) -> fields |> List.all check

  type Path = string list

  let pathToString (path : Path) : string =
    let pathStr = (path @ [ "val" ]) |> List.reverse |> String.concat "."
    $"in ({pathStr})"

  let rec letPatternEqualityBaseFn
    (checkIDs : bool)
    (path : Path)
    (actual : LetPattern)
    (expected : LetPattern)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let check path (a : 'a) (e : 'a) =
      if a <> e then errorFn path (string actual) (string expected)

    if checkIDs then check path (LetPattern.toID actual) (LetPattern.toID expected)

    match actual, expected with
    | LPVariable(_, name), LPVariable(_, name') -> check path name name'
    | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
      let all = first :: second :: theRest
      let all' = first' :: second' :: theRest'
      let zipped = List.zip all all'
      List.iter
        (fun (a, e) ->
          letPatternEqualityBaseFn checkIDs (path @ [ "tuple" ]) a e errorFn)
        zipped

    // exhaustive match
    | LPVariable _, _
    | LPTuple _, _ -> errorFn path (string actual) (string expected)


  let rec userTypeNameEqualityBaseFn
    (path : Path)
    (actual : TypeName.T)
    (expected : TypeName.T)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let err () = errorFn path (string actual) (string expected)

    match actual, expected with
    | FQName.BuiltIn a, FQName.BuiltIn e -> if a.name <> e.name then err ()
    | FQName.UserProgram a, FQName.UserProgram e ->
      if a.name <> e.name then err ()
      if a.version <> e.version then err ()
    | FQName.Package a, FQName.Package e ->
      if a.owner <> e.owner then err ()
      if a.modules <> e.modules then err ()
      if a.name <> e.name then err ()
      if a.version <> e.version then err ()
    | FQName.BuiltIn _, _
    | FQName.UserProgram _, _
    | FQName.Package _, _ -> err ()

  let rec matchPatternEqualityBaseFn
    (checkIDs : bool)
    (path : Path)
    (actual : MatchPattern)
    (expected : MatchPattern)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let eq path a e = matchPatternEqualityBaseFn checkIDs path a e errorFn

    let check path (a : 'a) (e : 'a) =
      if a <> e then errorFn path (string actual) (string expected)

    let eqList path (l1 : List<RT.MatchPattern>) (l2 : List<RT.MatchPattern>) =
      List.iteri2 (fun i l r -> eq (string i :: path) l r) l1 l2
      check path (List.length l1) (List.length l2)

    if checkIDs then
      check path (MatchPattern.toID actual) (MatchPattern.toID expected)

    match actual, expected with
    | MPVariable(_, name), MPVariable(_, name') -> check path name name'
    | (MPEnum(_, caseName, fieldPats), MPEnum(_, caseName', fieldPats')) ->
      check path caseName caseName'
      eqList (caseName :: path) fieldPats fieldPats'
    | MPString(_, str), MPString(_, str') -> check path str str'
    | MPInt(_, l), MPInt(_, l') -> check path l l'
    | MPFloat(_, d), MPFloat(_, d') -> check path d d'
    | MPBool(_, l), MPBool(_, l') -> check path l l'
    | MPChar(_, c), MPChar(_, c') -> check path c c'
    | MPUnit(_), MPUnit(_) -> ()
    | MPTuple(_, first, second, theRest), MPTuple(_, first', second', theRest') ->
      eqList path (first :: second :: theRest) (first' :: second' :: theRest')
    | MPList(_, pats), MPList(_, pats') -> eqList path pats pats'
    | MPListCons(_, head, tail), MPListCons(_, head', tail') ->
      check path head head'
      check path tail tail'
    // exhaustiveness check
    | MPVariable _, _
    | MPEnum _, _
    | MPString _, _
    | MPInt _, _
    | MPFloat _, _
    | MPBool _, _
    | MPChar _, _
    | MPUnit _, _
    | MPTuple _, _
    | MPListCons _, _
    | MPList _, _ -> check path actual expected



  let dTypeEqualityBaseFn
    (path : Path)
    (actual : TypeReference)
    (expected : TypeReference)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    // as long as TypeReferences don't get IDs, depending on structural equality is OK
    match actual, expected with
    | TInt, _
    | TFloat, _
    | TBool, _
    | TUnit, _
    | TString, _
    | TList(_), _
    | TTuple(_, _, _), _
    | TDict(_), _
    | TDB(_), _
    | TDateTime, _
    | TChar, _
    | TPassword, _
    | TUuid, _
    | TBytes, _
    | TVariable(_), _
    | TFn(_, _), _
    | TCustomType(_, _), _ ->
      if actual <> expected then errorFn path (string actual) (string expected)



  let rec exprEqualityBaseFn
    (checkIDs : bool)
    (path : Path)
    (actual : Expr)
    (expected : Expr)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let eq path a e = exprEqualityBaseFn checkIDs path a e errorFn

    let check path (a : 'a) (e : 'a) =
      if a <> e then errorFn path (string actual) (string expected)

    let eqList path (l1 : List<RT.Expr>) (l2 : List<RT.Expr>) =
      List.iteri2 (fun i l r -> eq (string i :: path) l r) l1 l2
      check path (List.length l1) (List.length l2)

    if checkIDs then check path (Expr.toID actual) (Expr.toID expected)

    match actual, expected with
    // expressions with no values
    | EUnit _, EUnit _ -> ()
    // expressions with single string values
    | EString(_, s), EString(_, s') ->
      let rec checkSegment s s' =
        match s, s' with
        | StringText s, StringText s' -> check path s s'
        | StringInterpolation e, StringInterpolation e' -> eq path e e'
        | _ -> check path s s'
      List.iter2 checkSegment s s'
    | EChar(_, v), EChar(_, v')
    | EVariable(_, v), EVariable(_, v') -> check path v v'
    | EInt(_, v), EInt(_, v') -> check path v v'
    | EFloat(_, v), EFloat(_, v') -> check path v v'
    | EBool(_, v), EBool(_, v') -> check path v v'
    | ELet(_, pat, rhs, body), ELet(_, pat', rhs', body') ->
      letPatternEqualityBaseFn checkIDs path pat pat' errorFn
      eq ("rhs" :: path) rhs rhs'
      eq ("body" :: path) body body'
    | EIf(_, con, thn, els), EIf(_, con', thn', els') ->
      eq ("cond" :: path) con con'
      eq ("then" :: path) thn thn'
      eq ("else" :: path) els els'
    | EList(_, l), EList(_, l') -> eqList path l l'
    | ETuple(_, first, second, theRest), ETuple(_, first', second', theRest') ->
      eq ("first" :: path) first first'
      eq ("second" :: path) second second'
      eqList path theRest theRest'

    | EApply(_, name, typeArgs, args), EApply(_, name', typeArgs', args') ->
      let path = (string name :: path)
      check path name name'

      check path (List.length typeArgs) (List.length typeArgs')
      List.iteri2
        (fun i l r -> dTypeEqualityBaseFn (string i :: path) l r errorFn)
        typeArgs
        typeArgs'

      eqList path args args'

    | ERecord(_, typeName, fields), ERecord(_, typeName', fields') ->
      userTypeNameEqualityBaseFn path typeName typeName' errorFn
      List.iter2
        (fun (k, v) (k', v') ->
          check path k k'
          eq (k :: path) v v')
        fields
        fields'
    | ERecordUpdate(_, record, updates), ERecordUpdate(_, record', updates') ->
      check path record record'
      List.iter2
        (fun (k, v) (k', v') ->
          check path k k'
          eq (k :: path) v v')
        updates
        updates'
    | EDict(_, fields), EDict(_, fields') ->
      List.iter2
        (fun (k, v) (k', v') ->
          check ("key" :: path) k k'
          eq ("value" :: path) v v')
        fields
        fields'

    | EFieldAccess(_, e, f), EFieldAccess(_, e', f') ->
      eq (f :: path) e e'
      check path f f'

    | EEnum(_, typeName, caseName, fields), EEnum(_, typeName', caseName', fields') ->
      userTypeNameEqualityBaseFn path typeName typeName' errorFn
      check path caseName caseName'
      eqList path fields fields'
      ()

    | ELambda(_, vars, e), ELambda(_, vars', e') ->
      let path = ("lambda" :: path)
      eq path e e'
      List.iteri2 (fun i (_, v) (_, v') -> check (string i :: path) v v') vars vars'
    | EMatch(_, e, branches), EMatch(_, e', branches') ->
      eq ("matchCond" :: path) e e'

      List.iter2
        (fun ((p, v) : MatchPattern * Expr) (p', v') ->
          matchPatternEqualityBaseFn checkIDs path p p' errorFn
          eq (string p :: path) v v')
        branches
        branches'
    | EAnd(_, l, r), EAnd(_, l', r') ->
      eq ("left" :: path) l l'
      eq ("right" :: path) r r'
    | EOr(_, l, r), EOr(_, l', r') ->
      eq ("left" :: path) l l'
      eq ("right" :: path) r r'
    | EError(_, msg, exprs), EError(_, msg', exprs') ->
      check path msg msg'
      eqList path exprs exprs'

    // exhaustiveness check
    | EUnit _, _
    | EInt _, _
    | EString _, _
    | EChar _, _
    | EVariable _, _
    | EBool _, _
    | EFloat _, _
    | ELet _, _
    | EIf _, _
    | EList _, _
    | ETuple _, _
    | EApply _, _
    | ERecord _, _
    | ERecordUpdate _, _
    | EDict _, _
    | EFieldAccess _, _
    | EEnum _, _
    | ELambda _, _
    | EMatch _, _
    | EAnd _, _
    | EOr _, _
    | EError _, _ -> check path actual expected



  // If the dvals are not the same, call errorFn. This is in this form to allow
  // both an equality function and a test expectation function
  let rec dvalEqualityBaseFn
    (path : Path)
    (actual : Dval)
    (expected : Dval)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let de p a e = dvalEqualityBaseFn p a e errorFn
    let error path = errorFn path (string actual) (string expected)

    let check (path : Path) (a : 'a) (e : 'a) : unit =
      if a <> e then errorFn path (debugDval actual) (debugDval expected)

    match actual, expected with
    | DFloat l, DFloat r ->
      if System.Double.IsNaN l && System.Double.IsNaN r then
        // This isn't "true" equality, it's just for tests
        ()
      else if
        System.Double.IsPositiveInfinity l && System.Double.IsPositiveInfinity r
      then
        ()
      else if
        System.Double.IsNegativeInfinity l && System.Double.IsNegativeInfinity r
      then
        ()
      else if not (Accuracy.areClose Accuracy.veryHigh l r) then
        error path
    | DDateTime l, DDateTime r ->
      // Two dates can be the same millisecond and not be equal if they don't
      // have the same number of ticks. For testing, we shall consider them
      // equal if they print the same string.
      check path (string l) (string r)
    | DList ls, DList rs ->
      check (".Length" :: path) (List.length ls) (List.length rs)
      List.iteri2 (fun i l r -> de (string i :: path) l r) ls rs

    | DTuple(firstL, secondL, theRestL), DTuple(firstR, secondR, theRestR) ->
      de path firstL firstR

      de path secondL secondR

      check (".Length" :: path) (List.length theRestL) (List.length theRestR)
      List.iteri2 (fun i l r -> de (string i :: path) l r) theRestL theRestR

    | DDict ls, DDict rs ->
      // check keys from ls are in both, check matching values
      Map.forEachWithIndex
        (fun key v1 ->
          match Map.tryFind key rs with
          | Some v2 -> de (key :: path) v1 v2
          | None -> check (key :: path) ls rs)
        ls
      // check keys from rs are in both
      Map.forEachWithIndex
        (fun key _ ->
          match Map.tryFind key rs with
          | Some _ -> () // already checked
          | None -> check (key :: path) ls rs)
        rs
      check (".Length" :: path) (Map.count ls) (Map.count rs)

    | DRecord(ltn, ls), DRecord(rtn, rs) ->
      userTypeNameEqualityBaseFn path ltn rtn errorFn
      // check keys from ls are in both, check matching values
      Map.forEachWithIndex
        (fun key v1 ->
          match Map.tryFind key rs with
          | Some v2 -> de (key :: path) v1 v2
          | None -> check (key :: path) ls rs)
        ls
      // check keys from rs are in both
      Map.forEachWithIndex
        (fun key _ ->
          match Map.tryFind key rs with
          | Some _ -> () // already checked
          | None -> check (key :: path) ls rs)
        rs
      check (".Length" :: path) (Map.count ls) (Map.count rs)


    | DEnum(typeName, caseName, fields), DEnum(typeName', caseName', fields') ->
      userTypeNameEqualityBaseFn path typeName typeName' errorFn
      check ("caseName" :: path) caseName caseName'

      check ("fields.Length" :: path) (List.length fields) (List.length fields)
      List.iteri2 (fun i l r -> de (string i :: path) l r) fields fields'
      ()

    | DIncomplete _, DIncomplete _ -> ()
    | DError(_, msg1), DError(_, msg2) ->
      check path (msg1.Replace("_v0", "")) (msg2.Replace("_v0", ""))
    | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
      let vals l = List.map Tuple2.second l
      check ("lambdaVars" :: path) (vals l1.parameters) (vals l2.parameters)
      check ("symbtable" :: path) l1.symtable l2.symtable // TODO: use dvalEquality
      exprEqualityBaseFn false path l1.body l2.body errorFn
    | DString _, DString _ -> check path (debugDval actual) (debugDval expected)
    // Keep for exhaustiveness checking
    | DDict _, _
    | DRecord _, _
    | DEnum _, _
    | DList _, _
    | DTuple _, _
    | DString _, _
    | DInt _, _
    | DDateTime _, _
    | DBool _, _
    | DFloat _, _
    | DUnit, _
    | DChar _, _
    | DPassword _, _
    | DFnVal _, _
    | DIncomplete _, _
    | DError _, _
    | DDB _, _
    | DUuid _, _
    | DBytes _, _ -> check path actual expected

  let rec equalDval (actual : Dval) (expected : Dval) (msg : string) : unit =
    dvalEqualityBaseFn [] actual expected (fun path a e ->
      Expect.equal a e $"{msg}: {pathToString path} (overall: {actual})")

  let rec equalMatchPattern
    (actual : MatchPattern)
    (expected : MatchPattern)
    (msg : string)
    : unit =
    matchPatternEqualityBaseFn true [] actual expected (fun path a e ->
      Expect.equal a e $"{msg}: {pathToString path}")

  let rec equalMatchPatternIgnoringIDs
    (actual : MatchPattern)
    (expected : MatchPattern)
    : unit =
    matchPatternEqualityBaseFn false [] actual expected (fun path a e ->
      Expect.equal a e (pathToString path))

  let rec equalExpr
    (types : Types)
    (actual : Expr)
    (expected : Expr)
    (msg : string)
    : unit =
    exprEqualityBaseFn true [] actual expected (fun path a e ->
      Expect.equal a e $"{msg}: {pathToString path}")

  let rec equalExprIgnoringIDs (actual : Expr) (expected : Expr) : unit =
    exprEqualityBaseFn false [] actual expected (fun path a e ->
      Expect.equal a e (pathToString path))

  let dvalEquality (left : Dval) (right : Dval) : bool =
    let success = ref true
    dvalEqualityBaseFn [] left right (fun _ _ _ -> success.Value <- false)
    success.Value

  let dvalMapEquality (m1 : DvalMap) (m2 : DvalMap) =
    dvalEquality (DDict m1) (DDict m2)

let visitDval (f : Dval -> 'a) (dv : Dval) : List<'a> =
  let mutable state = []
  let f dv = state <- f dv :: state
  let rec visit dv : unit =
    match dv with
    // Keep for exhaustiveness checking
    | DDict map -> Map.values map |> List.map visit |> ignore<List<unit>>
    | DRecord(_, map) -> Map.values map |> List.map visit |> ignore<List<unit>>
    | DEnum(_typeName, _caseName, fields) ->
      fields |> List.map visit |> ignore<List<unit>>
    | DList dvs -> List.map visit dvs |> ignore<List<unit>>
    | DTuple(first, second, theRest) ->
      List.map visit ([ first; second ] @ theRest) |> ignore<List<unit>>
    | DString _
    | DInt _
    | DDateTime _
    | DBool _
    | DFloat _
    | DUnit
    | DChar _
    | DPassword _
    | DFnVal _
    | DIncomplete _
    | DError _
    | DDB _
    | DUuid _
    | DBytes _
    | DString _ -> f dv
    f dv
  visit dv
  state

let containsPassword (dv : Dval) : bool =
  let isPassword dval =
    match dval with
    | RT.DPassword _ -> true
    | _ -> false
  dv |> visitDval isPassword |> List.any Fun.identity

let containsFakeDval (dv : Dval) : bool =
  dv |> visitDval RT.Dval.isFake |> List.any Fun.identity


let interestingStrings : List<string * string> =
  [ ("arabic", "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽")
    ("emoji0", "🧟‍♀️🧟‍♂️🧟‍♀️🧑🏽‍🦰")
    ("emoji1", "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷")
    ("emoji2", "🧑🏽‍🦰‍🧑🏼‍💻‍‍")
    ("unicode4", "Είναι προικισμένοι με λογική")
    ("skin tone", "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿")
    // ("zalgo", "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇")
    ("escaped", "\u0014\u0004")
    ("zolw", "żółw")
    ("hope in greek", "ελπίδα")
    ("html", "<html><head></head><body><h1>title</h1></body></html>")
    ("accents", "óñÜáâȺ") ]



let interestingFloats : List<string * float> =
  let initial =
    // interesting cause OCaml uses 31 bit ints
    [ "min 31 bit", System.Math.Pow(2.0, 30.0) - 1.0
      "max 31 bit", - System.Math.Pow(2.0, 30.0)
      // interesting cause boundary of 32 bit ints
      "min 32 bit", System.Math.Pow(2.0, 31.0) - 1.0
      "max 32 bit", - System.Math.Pow(2.0, 31.0)
      // interesting cause doubles support up to 53-bit ints
      "min 53 bit", System.Math.Pow(2.0, 52.0) - 1.0
      "max 53 bit", - System.Math.Pow(2.0, 52.0)
      // interesting cause OCaml uses 63 bit ints
      "min 63 bit", System.Math.Pow(2.0, 62.0) - 1.0
      "max 63 bit", - System.Math.Pow(2.0, 62.0)
      // interesting cause boundary of 64 bit ints
      "min 64 bit", System.Math.Pow(2.0, 63.0) - 1.0
      "max 64 bit", - System.Math.Pow(2.0, 63.0)
      // Interesting anyway
      "zero", 0.0
      "negative zero", -0.0
      "NaN", nan
      "infinity", infinity
      "-infinity", -infinity
      // Mathy values
      "e", System.Math.E
      "pi", System.Math.PI
      "tau", System.Math.Tau ]

  initial
  |> List.flatMap (fun (doc, v) ->
    [ ($"float {doc} - 1", v - 1.0); ($"{doc} + 0", v); ($"{doc} + 1", v + 1.0) ])

let interestingInts : List<string * int64> =
  [ ("int0", 0L)
    ("int1", 1L)
    ("int-1", -1L)
    ("int_max_31_bits", 1073741823L) // 2^30-1
    ("int_min_31_bits", -1073741824L) // -2^30
    ("int_max_32_bits", 2147483647L) // 2^31-1
    ("int_min_32_bits", -2147483648L) // 2^31-1
    ("int_max_63_bits", 4611686018427387903L) // 2^62-1
    ("int_min_63_bits", -4611686018427387904L) // -2^62
    ("int_max_64_bits", 9223372036854775807L) // 2^63-1
    ("int_min_64_bits", -9223372036854775808L) // -2^63
    ("int_max_double", 9007199254740992L) // 2^53
    ("int_min_double", -9007199254740992L) ] // -2^53
  |> List.flatMap (fun (doc, v) ->
    [ ($"int {doc} - 1", v - 1L); ($"{doc} + 0", v); ($"{doc} + 1", v + 1L) ])


// https://github.com/minimaxir/big-list-of-naughty-strings
let naughtyStrings : List<string * string> =
  LibBackend.File.readfile LibBackend.Config.Testdata "naughty-strings.txt"
  |> String.splitOnNewline
  |> List.mapWithIndex (fun i s -> $"naughty string line {i + 1}", s)
  // 139 is the Unicode BOM on line 140, which is tough to get .NET to put in a string
  |> List.filterWithIndex (fun i (_, str) ->
    i <> 139 && not (String.startsWith "#" str))


let interestingDvals : List<string * RT.Dval * RT.TypeReference> =
  [ ("float", DFloat 7.2, TFloat)
    ("float2", DFloat -7.2, TFloat)
    ("float3", DFloat 15.0, TFloat)
    ("float4", DFloat -15.0, TFloat)
    ("int5", DInt 5L, TInt)
    ("true", DBool true, TBool)
    ("false", DBool false, TBool)
    ("unit", DUnit, TUnit)
    ("datastore", DDB "Visitors", TDB TInt)
    ("string", DString "incredibly this was broken", TString)
    // Json.NET has a habit of converting things automatically based on the type in the string
    ("date string", DString "2018-09-14T00:31:41Z", TString)
    ("int string", DString "1039485", TString)
    ("int string2", DString "-1039485", TString)
    ("int string3", DString "0", TString)
    ("uuid string", DString "7d9e5495-b068-4364-a2cc-3633ab4d13e6", TString)
    ("list", DList [ Dval.int 4 ], TList TInt)
    ("list with derror",
     DList [ Dval.int 3; DError(SourceNone, "some error string"); Dval.int 4 ],
     TList TInt)
    ("record",
     DRecord(
       S.fqUserTypeName [ "Two"; "Modules" ] "Foo" 0,
       Map.ofList [ "foo", Dval.int 5 ]
     ),
     TCustomType(S.fqUserTypeName [ "Two"; "Modules" ] "Foo" 0, []))
    ("record2",
     DRecord(
       S.fqUserTypeName [] "Foo" 0,
       Map.ofList [ ("type", DString "weird"); ("value", DUnit) ]
     ),
     TCustomType(S.fqUserTypeName [] "Foo" 0, []))
    ("record3",
     DRecord(
       S.fqUserTypeName [] "Foo" 0,
       Map.ofList [ ("type", DString "weird"); ("value", DString "x") ]
     ),
     TCustomType(S.fqUserTypeName [] "Foo" 0, []))
    // More Json.NET tests
    ("record4",
     DRecord(S.fqUserTypeName [] "Foo" 0, Map.ofList [ "foo\\\\bar", Dval.int 5 ]),
     TCustomType(S.fqUserTypeName [] "Foo" 0, []))
    ("record5",
     DRecord(S.fqUserTypeName [] "Foo" 0, Map.ofList [ "$type", Dval.int 5 ]),
     TCustomType(S.fqUserTypeName [] "Foo" 0, []))
    ("record with error",
     DRecord(
       S.fqUserTypeName [] "Foo" 0,
       Map.ofList [ "v", DError(SourceNone, "some error string") ]
     ),
     TCustomType(S.fqUserTypeName [] "Foo" 0, []))
    ("dict", DDict(Map.ofList [ "foo", Dval.int 5 ]), TDict TInt)
    ("dict3",
     DDict(Map.ofList [ ("type", DString "weird"); ("value", DString "x") ]),
     TDict TString)
    // More Json.NET tests
    ("dict4", DDict(Map.ofList [ "foo\\\\bar", Dval.int 5 ]), TDict TInt)
    ("dict5", DDict(Map.ofList [ "$type", Dval.int 5 ]), TDict TInt)
    ("dict with error",
     DDict(Map.ofList [ "v", DError(SourceNone, "some error string") ]),
     TDict TInt)
    ("incomplete", DIncomplete SourceNone, TInt)
    ("incomplete2", DIncomplete(SourceID(14219007199254740993UL, 8UL)), TBool)
    ("error", DError(SourceNone, "some error string"), TString)
    ("lambda",
     DFnVal(
       Lambda
         { body = RT.EUnit(id 1234)
           symtable = Map.empty
           parameters = [ (id 5678, "a") ] }
     ),
     TFn([ TInt ], TUnit))
    ("lambda with pipe",
     DFnVal(
       Lambda
         { body =
             EApply(
               92356985UL,
               (FnTargetName(
                 FQName.BuiltIn
                   { modules = [ "List" ]; name = FnName.FnName "push"; version = 0 }
               )),
               [],
               [ EApply(
                   93459985UL,
                   (FnTargetName(
                     FQName.BuiltIn
                       { modules = []; name = FnName.FnName "+"; version = 0 }
                   )),
                   [],
                   [ EApply(
                       394567785UL,
                       (FnTargetName(
                         FQName.BuiltIn
                           { modules = []; name = FnName.FnName "+"; version = 0 }
                       )),
                       [],
                       [ EApply(
                           44444485UL,
                           (FnTargetName(
                             FQName.BuiltIn
                               { modules = []
                                 name = FnName.FnName "+"
                                 version = 0 }
                           )),
                           [],
                           [ EInt(234213618UL, 5); EInt(923423468UL, 6) ]
                         )
                         EInt(648327618UL, 7) ]
                     )
                     EInt(325843618UL, 8) ]
                 ) ]
             )
           symtable = Map.empty
           parameters = [ (id 5678, "a") ] }
     ),
     TFn([ TInt ], TInt))
    ("db", DDB "Visitors", TDB TInt)
    ("date",
     DDateTime(
       DarkDateTime.fromInstant (NodaTime.Instant.ofIsoString "2018-09-14T00:31:41Z")
     ),
     TDateTime)
    ("password", DPassword(Password(UTF8.toBytes "somebytes")), TPassword)
    ("uuid", DUuid(System.Guid.Parse "7d9e5495-b068-4364-a2cc-3633ab4d13e6"), TUuid)
    ("uuid0", DUuid(System.Guid.Parse "00000000-0000-0000-0000-000000000000"), TUuid)
    ("option", Dval.optionNothing, TypeReference.option TInt)
    ("option2", Dval.optionJust (Dval.int 15), TypeReference.option TInt)
    ("option3", Dval.optionJust (DString "a string"), TypeReference.option TString)
    ("character", DChar "s", TChar)
    ("bytes", "JyIoXCg=" |> System.Convert.FromBase64String |> DBytes, TBytes)
    // use image bytes here to test for any weird bytes forms
    ("bytes2",
     DBytes(
       LibBackend.File.readfileBytes
         LibBackend.Config.Testdata
         "sample_image_bytes.png"
     // TODO: deeply nested data
     ),
     TBytes)

    ("simple2Tuple", DTuple(Dval.int 1, Dval.int 2, []), TTuple(TInt, TInt, []))
    ("simple3Tuple",
     DTuple(Dval.int 1, Dval.int 2, [ Dval.int 3 ]),
     TTuple(TInt, TInt, [ TInt ]))
    ("tupleWithUnit",
     DTuple(Dval.int 1, Dval.int 2, [ DUnit ]),
     TTuple(TInt, TInt, [ TUnit ]))
    ("tupleWithError",
     DTuple(Dval.int 1, Dval.resultError (DString "error"), []),
     TTuple(TInt, TypeReference.result TInt TString, [])) ]

let sampleDvals : List<string * (Dval * TypeReference)> =
  (List.map (fun (k, v) -> k, DInt v, TInt) interestingInts
   @ List.map (fun (k, v) -> k, DFloat v, TFloat) interestingFloats
   @ List.map (fun (k, v) -> k, DString v, TString) interestingStrings
   @ List.map (fun (k, v) -> k, DString v, TString) naughtyStrings
   @ interestingDvals)
  |> List.map (fun (k, v, t) -> k, (v, t))

// Utilties shared among tests
module Http =
  type T = { status : string; headers : (string * string) list; body : byte array }

  let setHeadersToCRLF (text : byte array) : byte array =
    // We keep our test files with an LF line ending, but the HTTP spec
    // requires headers (but not the body, nor the first line) to have CRLF
    // line endings
    let mutable justSawNewline = false
    let mutable inBody = false

    text
    |> Array.toList
    |> List.collect (fun b ->
      if not inBody && b = byte '\n' then
        if justSawNewline then inBody <- true
        justSawNewline <- true
        [ byte '\r'; b ]
      else
        justSawNewline <- false
        [ b ])
    |> List.toArray

  let split (response : byte array) : T =
    // read a single line of bytes (a line ends with \r\n)
    let rec consume (existing : byte list) (l : byte list) : byte list * byte list =
      match l with
      | [] -> [], []
      | 13uy :: 10uy :: tail -> existing, tail
      | head :: tail -> consume (existing @ [ head ]) tail

    // read all headers (ends when we get two \r\n in a row), return headers
    // and remaining byte string (the body). Assumes the status line is not
    // present. Headers are returned reversed
    let rec consumeHeaders
      (headers : string list)
      (l : byte list)
      : string list * byte list =
      let (line, remaining) = consume [] l

      if line = [] then
        (headers, remaining)
      else
        let str = line |> Array.ofList |> UTF8.ofBytesUnsafe
        consumeHeaders (str :: headers) remaining

    let bytes = Array.toList response

    // read the status like (eg HTTP 200 OK)
    let status, bytes = consume [] bytes

    let headers, body = consumeHeaders [] bytes

    let headers =
      headers
      |> List.reverse
      |> List.map (fun s ->
        match String.split ":" s with
        | k :: vs -> (k, vs |> String.concat ":" |> String.trimLeft)
        | _ -> Exception.raiseInternal $"not a valid header" [ "header", s ])


    { status = status |> List.toArray |> UTF8.ofBytesUnsafe
      headers = headers
      body = List.toArray body }

// For an ASP.NET http server, remove the default loggers and add a file logger that
// saves the output in rundir/logs
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open NReco.Logging.File

let configureLogging
  (name : string)
  (builder : Microsoft.Extensions.Logging.ILoggingBuilder)
  : unit =
  // This removes the default ConsoleLogger. Having two console loggers (this one and
  // also the one in Main), caused a deadlock (possibly from having two different
  // console logging threads).
  builder
    .ClearProviders()
    .Services.AddLogging(fun loggingBuilder ->
      loggingBuilder.AddFile(
        $"{LibBackend.Config.logDir}{name}.log",
        append = false
      )
      |> ignore<ILoggingBuilder>)
  |> ignore<IServiceCollection>
