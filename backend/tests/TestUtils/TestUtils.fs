module TestUtils.TestUtils

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open LibService.Exception

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Exe = LibExecution.Execution
module S = RTShortcuts

let testOwner : Lazy<Task<Account.UserInfo>> =
  lazy
    (UserName.create "test"
     |> Account.getUser
     |> Task.map (Exception.unwrapOptionInternal "Could not get testOwner user" []))

/// Delete test data (in DB) for one canvas
let clearCanvasData (owner : UserID) (name : CanvasName.T) : Task<unit> =
  task {
    let! canvasID = Canvas.canvasIDForCanvasName owner name

    let cronRecords =
      Sql.query "DELETE FROM cron_records where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let customDomains =
      Sql.query "DELETE FROM custom_domains where canvas = @name"
      |> Sql.parameters [ "name", Sql.string (string name) ]
      |> Sql.executeStatementAsync

    let events =
      Sql.query "DELETE FROM events where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let eventsV2 =
      Sql.query "DELETE FROM events_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let functionArguments =
      Sql.query "DELETE FROM function_arguments where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let functionResultsV3 =
      Sql.query "DELETE FROM function_results_v3 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let schedulingRules =
      Sql.query "DELETE FROM scheduling_rules where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let secrets =
      Sql.query "DELETE FROM secrets where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let staticAssetDeploys =
      Sql.query "DELETE FROM static_asset_deploys where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let storedEventsV2 =
      Sql.query "DELETE FROM stored_events_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let toplevelOplists =
      Sql.query "DELETE FROM toplevel_oplists where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let userData =
      Sql.query "DELETE FROM user_data where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    let! (_ : List<unit>) =
      Task.flatten [ cronRecords
                     customDomains
                     events
                     eventsV2
                     functionArguments
                     functionResultsV3
                     schedulingRules
                     secrets
                     staticAssetDeploys
                     storedEventsV2
                     toplevelOplists
                     userData ]

    return ()
  }

type TestCanvasName =
  /// Use exactly this canvas name for the test. This is needed to test some features
  /// which use the canvas name within them, such as static assets.
  | Exact of string
  /// Randomized test name using the provided base. This allows tests to avoid
  /// sharing the same canvas so they can be parallelized, etc
  | Randomized of string

let nameToTestName (name : TestCanvasName) : string =
  match name with
  | Exact name -> name
  | Randomized name ->
    let name =
      name
      |> String.toLowercase
      // replace invalid chars with a single hyphen
      |> FsRegEx.replace "[^-a-z0-9]+" "-"
      |> FsRegEx.replace "[-_]+" "-"
      |> String.take 50
    let suffix = randomString 5 |> String.toLowercase
    $"test-{name}-{suffix}" |> FsRegEx.replace "[-_]+" "-"

let initializeCanvasForOwner
  (owner : Account.UserInfo)
  (name : TestCanvasName)
  : Task<Canvas.Meta> =
  task {
    let canvasName = CanvasName.createExn (nameToTestName name)
    do! clearCanvasData owner.id canvasName
    let! id = Canvas.canvasIDForCanvasName owner.id canvasName
    return { id = id; name = canvasName; owner = owner.id }
  }

let initializeTestCanvas (name : TestCanvasName) : Task<Canvas.Meta> =
  task {
    let! owner = testOwner.Force()
    return! initializeCanvasForOwner owner name
  }


// Same as initializeTestCanvas, for tests that don't need to hit the DB
let createCanvasForOwner
  (owner : Account.UserInfo)
  (name : TestCanvasName)
  : Task<Canvas.Meta> =
  task {
    let canvasName = CanvasName.createExn (nameToTestName name)
    let id = System.Guid.NewGuid()
    return { id = id; name = canvasName; owner = owner.id }
  }

let createTestCanvas (name : TestCanvasName) : Task<Canvas.Meta> =
  task {
    let! owner = testOwner.Force()
    return! createCanvasForOwner owner name
  }

let testPos : PT.Position = { x = 0; y = 0 }

let testHttpRouteHandler
  (route : string)
  (method : string)
  (ast : PT.Expr)
  : PT.Handler.T =
  let ids : PT.Handler.ids =
    { moduleID = gid (); nameID = gid (); modifierID = gid () }

  { pos = { x = 0; y = 0 }
    tlid = gid ()
    ast = ast
    spec = PT.Handler.HTTP(route, method, ids) }

let testCron
  (name : string)
  (interval : PT.Handler.CronInterval)
  (ast : PT.Expr)
  : PT.Handler.T =
  let ids : PT.Handler.ids =
    { moduleID = gid (); nameID = gid (); modifierID = gid () }

  { pos = { x = 0; y = 0 }
    tlid = gid ()
    ast = ast
    spec = PT.Handler.Cron(name, Some interval, ids) }

let testWorker (name : string) (ast : PT.Expr) : PT.Handler.T =
  let ids : PT.Handler.ids =
    { moduleID = gid (); nameID = gid (); modifierID = gid () }

  { pos = { x = 0; y = 0 }
    tlid = gid ()
    ast = ast
    spec = PT.Handler.Worker(name, ids) }

let testUserFn
  (name : string)
  (parameters : string list)
  (body : PT.Expr)
  : PT.UserFunction.T =
  { tlid = gid ()
    body = body
    description = ""
    infix = false
    name = name
    nameID = gid ()
    returnType = PT.TVariable "a"
    returnTypeID = gid ()
    parameters =
      List.map
        (fun (p : string) ->
          { name = p
            nameID = gid ()
            typ = Some(PT.TVariable "b")
            typeID = gid ()
            description = "test" })
        parameters }

let testUserType
  (name : string)
  (definition : List<string * PT.DType>)
  : PT.UserType.T =
  { tlid = gid ()
    name = name
    nameID = gid ()
    version = 0
    definition =
      definition
      |> List.map (fun (name, typ) ->
        ({ name = name; typ = Some typ; typeID = gid (); nameID = gid () } : PT.UserType.RecordField))
      |> PT.UserType.Record }



let handlerOp (h : PT.Handler.T) = PT.SetHandler(h.tlid, h.pos, h)

let testDBCol (name : Option<string>) (typ : Option<PT.DType>) : PT.DB.Col =
  { name = name; typ = typ; nameID = gid (); typeID = gid () }

let testDB (name : string) (cols : List<PT.DB.Col>) : PT.DB.T =
  { tlid = gid ()
    pos = { x = 0; y = 0 }
    nameID = gid ()
    name = name
    cols = cols
    version = 0 }

/// Library function to be usable within tests.
/// Includes normal StdLib fns, as well as test-specific fns.
/// In the case of a fn existing in both places, the test fn is the one used.
let libraries : Lazy<RT.Libraries> =
  lazy
    (let testFns =
      LibTest.fns
      |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)
      |> Map.mergeFavoringLeft LibRealExecution.RealExecution.stdlibFns
     { stdlib = testFns; packageFns = Map.empty })

let executionStateFor
  (meta : Canvas.Meta)
  (dbs : Map<string, RT.DB.T>)
  (userFunctions : Map<string, RT.UserFunction.T>)
  : Task<RT.ExecutionState> =
  task {
    let program : RT.ProgramContext =
      { canvasID = meta.id
        canvasName = meta.name
        accountID = meta.owner
        userFns = userFunctions
        dbs = dbs
        userTypes = Map.empty
        secrets = [] }

    let testContext : RT.TestContext =
      { sideEffectCount = 0
        exceptionReports = []
        expectedExceptionCount = 0
        postTestExecutionHook =
          fun tc result ->
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
                $"UNEXPECTED EXCEPTION COUNT in test {meta.name}"
                [ "expectedExceptionCount", tc.expectedExceptionCount
                  "actualExceptionCount", tc.exceptionReports.Length ] }

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
        (Lazy.force libraries)
        (Exe.noTracing RT.Real)
        exceptionReporter
        notifier
        (id 7)
        program
    let state = { state with test = testContext }
    return state
  }

/// Saves and reloads the canvas for the Toplevels
let canvasForTLs (meta : Canvas.Meta) (tls : List<PT.Toplevel.T>) : Task<Canvas.T> =
  task {
    let descs =
      tls
      |> List.map (fun tl ->
        let tlid = PT.Toplevel.toTLID tl

        let op =
          match tl with
          | PT.Toplevel.TLHandler h -> PT.SetHandler(h.tlid, { x = 0; y = 0 }, h)
          | _ -> Exception.raiseInternal "not yet supported in canvasForTLs" []

        (tlid, [ op ], tl, Canvas.NotDeleted))

    do! Canvas.saveTLIDs meta descs
    return! Canvas.loadAll meta
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

// Remove random things like IDs to make the tests stable
let normalizeDvalResult (dv : RT.Dval) : RT.Dval =
  match dv with
  | RT.DError (_, str) -> RT.DError(RT.SourceNone, str)
  | RT.DIncomplete _ -> RT.DIncomplete(RT.SourceNone)
  | dv -> dv

open LibExecution.RuntimeTypes

let rec debugDval (v : Dval) : string =
  match v with
  | DStr s ->
    $"DStr '{s}'(len {s.Length}, {System.BitConverter.ToString(UTF8.toBytes s)})"
  | DDate d ->
    $"DDate '{DDateTime.toIsoString d}': (millies {d.InUtc().Millisecond})"
  | DObj obj ->
    obj
    |> Map.toList
    |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
    |> String.concat ",\n  "
    |> fun contents -> $"DObj {{\n  {contents}}}"
  | DBytes b ->
    b
    |> Array.toList
    |> List.map string
    |> String.concat ", "
    |> fun s -> $"[|{s}|]"

  | _ -> v.ToString()

module Expect =
  // Checks if the value (and all its contents) is in its desired
  // representation (in the event that there are multiple ways to represent
  // it). Think of this as a general form of string normalization.
  let rec isCanonical (dv : Dval) : bool =
    let check = isCanonical

    match dv with
    | DDate _
    | DIncomplete _
    | DInt _
    | DDate _
    | DBool _
    | DFloat _
    | DNull
    | DPassword _
    | DFnVal _
    | DError _
    | DDB _
    | DUuid _
    | DBytes _
    | DOption None
    | DFloat _ -> true

    | DHttpResponse (Redirect str) -> str.IsNormalized()
    | DHttpResponse (Response (_, headers, v)) ->
      // We don't check code as you can actually set it to anything
      let vOk = check v

      let headersOk =
        List.all
          (fun ((k, v) : string * string) -> k.IsNormalized() && v.IsNormalized())
          headers

      vOk && headersOk

    | DResult (Ok v)
    | DResult (Error v)
    | DErrorRail v
    | DOption (Some v) -> check v

    | DList vs -> List.all check vs
    | DTuple (first, second, rest) -> List.all check ([ first; second ] @ rest)
    | DObj vs -> vs |> Map.values |> List.all check
    | DStr str -> str.IsNormalized()
    | DChar str -> str.IsNormalized() && String.lengthInEgcs str = 1

  type Path = string list

  let pathToString (path : Path) : string =
    let pathStr = (path @ [ "val" ]) |> List.reverse |> String.concat "."
    $"in ({pathStr})"

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
    | MPVariable (_, name), MPVariable (_, name') -> check path name name'
    | (MPConstructor (_, name, patterns), MPConstructor (_, name', patterns')) ->
      check path name name'
      eqList (name :: path) patterns patterns'
    | MPString (_, str), MPString (_, str') -> check path str str'
    | MPInteger (_, l), MPInteger (_, l') -> check path l l'
    | MPFloat (_, d), MPFloat (_, d') -> check path d d'
    | MPBool (_, l), MPBool (_, l') -> check path l l'
    | MPCharacter (_, c), MPCharacter (_, c') -> check path c c'
    | MPNull (_), MPNull (_) -> ()
    | MPBlank (_), MPBlank (_) -> ()
    | MPTuple (_, first, second, theRest), MPTuple (_, first', second', theRest') ->
      eqList path (first :: second :: theRest) (first' :: second' :: theRest')
    // exhaustiveness check
    | MPVariable _, _
    | MPConstructor _, _
    | MPString _, _
    | MPInteger _, _
    | MPFloat _, _
    | MPBool _, _
    | MPCharacter _, _
    | MPNull _, _
    | MPBlank _, _
    | MPTuple _, _ -> check path actual expected


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
    | ENull _, ENull _
    | EBlank _, EBlank _ -> ()
    // expressions with single string values
    | EString (_, v), EString (_, v')
    | ECharacter (_, v), ECharacter (_, v')
    | EVariable (_, v), EVariable (_, v') -> check path v v'
    | EInteger (_, v), EInteger (_, v') -> check path v v'
    | EFloat (_, v), EFloat (_, v') -> check path v v'
    | EBool (_, v), EBool (_, v') -> check path v v'
    | ELet (_, lhs, rhs, body), ELet (_, lhs', rhs', body') ->
      check path lhs lhs'
      eq ("rhs" :: path) rhs rhs'
      eq ("body" :: path) body body'
    | EIf (_, con, thn, els), EIf (_, con', thn', els') ->
      eq ("cond" :: path) con con'
      eq ("then" :: path) thn thn'
      eq ("else" :: path) els els'
    | EList (_, l), EList (_, l') -> eqList path l l'
    | ETuple (_, first, second, theRest), ETuple (_, first', second', theRest') ->
      eq ("first" :: path) first first'
      eq ("second" :: path) second second'
      eqList path theRest theRest'
    | EFQFnValue (_, v), EFQFnValue (_, v') -> check path v v'
    | EApply (_, name, args, inPipe, toRail),
      EApply (_, name', args', inPipe', toRail') ->
      let path = (string name :: path)
      eq path name name'
      eqList path args args'

      match (inPipe, inPipe') with
      | InPipe id, InPipe id' -> if checkIDs then check path id id'
      | _ -> check path inPipe inPipe'

      check path toRail toRail'

    | ERecord (_, pairs), ERecord (_, pairs') ->
      List.iter2
        (fun (k, v) (k', v') ->
          check path k k'
          eq (k :: path) v v')
        pairs
        pairs'
    | EFieldAccess (_, e, f), EFieldAccess (_, e', f') ->
      eq (f :: path) e e'
      check path f f'
    | EFeatureFlag (_, cond, old, knew), EFeatureFlag (_, cond', old', knew') ->
      eq ("flagCond" :: path) cond cond'
      eq ("flagOld" :: path) old old'
      eq ("flagNew" :: path) knew knew'
    | EConstructor (_, s, ts), EConstructor (_, s', ts') ->
      check path s s'
      eqList (s :: path) ts ts'
    | ELambda (_, vars, e), ELambda (_, vars', e') ->
      let path = ("lambda" :: path)
      eq path e e'
      List.iteri2 (fun i (_, v) (_, v') -> check (string i :: path) v v') vars vars'
    | EMatch (_, e, branches), EMatch (_, e', branches') ->
      eq ("matchCond" :: path) e e'

      List.iter2
        (fun ((p, v) : MatchPattern * Expr) (p', v') ->
          matchPatternEqualityBaseFn checkIDs path p p' errorFn
          eq (string p :: path) v v')
        branches
        branches'
    | EAnd (_, l, r), EAnd (_, l', r') ->
      eq ("left" :: path) l l'
      eq ("right" :: path) r r'
    | EOr (_, l, r), EOr (_, l', r') ->
      eq ("left" :: path) l l'
      eq ("right" :: path) r r'

    // exhaustiveness check
    | ENull _, _
    | EBlank _, _
    | EInteger _, _
    | EString _, _
    | ECharacter _, _
    | EVariable _, _
    | EBool _, _
    | EFloat _, _
    | ELet _, _
    | EIf _, _
    | EList _, _
    | ETuple _, _
    | EFQFnValue _, _
    | EApply _, _
    | ERecord _, _
    | EFieldAccess _, _
    | EFeatureFlag _, _
    | EConstructor _, _
    | ELambda _, _
    | EMatch _, _
    | EAnd _, _
    | EOr _, _ -> check path actual expected



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
      else if System.Double.IsPositiveInfinity l
              && System.Double.IsPositiveInfinity r then
        ()
      else if System.Double.IsNegativeInfinity l
              && System.Double.IsNegativeInfinity r then
        ()
      else if not (Accuracy.areClose Accuracy.veryHigh l r) then
        error path
    | DResult (Ok l), DResult (Ok r) -> de ("Ok" :: path) l r
    | DResult (Error l), DResult (Error r) -> de ("Error" :: path) l r
    | DOption (Some l), DOption (Some r) -> de ("Just" :: path) l r
    | DDate l, DDate r ->
      // Two dates can be the same millisecond and not be equal if they don't
      // have the same number of ticks. For testing, we shall consider them
      // equal if they print the same string.
      check path (string l) (string r)
    | DList ls, DList rs ->
      check (".Length" :: path) (List.length ls) (List.length rs)
      List.iteri2 (fun i l r -> de (string i :: path) l r) ls rs

    | DTuple (firstL, secondL, theRestL), DTuple (firstR, secondR, theRestR) ->
      de path firstL firstR

      de path secondL secondR

      check (".Length" :: path) (List.length theRestL) (List.length theRestR)
      List.iteri2 (fun i l r -> de (string i :: path) l r) theRestL theRestR

    | DObj ls, DObj rs ->
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
    | DHttpResponse (Response (sc1, h1, b1)), DHttpResponse (Response (sc2, h2, b2)) ->
      check path sc1 sc2
      check path h1 h2
      de ("response" :: path) b1 b2
    | DHttpResponse (Redirect u1), DHttpResponse (Redirect u2) ->
      check ("redirectUrl" :: path) u1 u2
    | DIncomplete _, DIncomplete _ -> ()
    | DError (_, msg1), DError (_, msg2) ->
      check path (msg1.Replace("_v0", "")) (msg2.Replace("_v0", ""))
    | DErrorRail l, DErrorRail r -> de ("ErrorRail" :: path) l r
    | DFnVal (Lambda l1), DFnVal (Lambda l2) ->
      let vals l = List.map Tuple2.second l
      check ("lambdaVars" :: path) (vals l1.parameters) (vals l2.parameters)
      check ("symbtable" :: path) l1.symtable l2.symtable // TODO: use dvalEquality
      exprEqualityBaseFn false path l1.body l2.body errorFn
    | DStr _, DStr _ -> check path (debugDval actual) (debugDval expected)
    // Keep for exhaustiveness checking
    | DHttpResponse _, _
    | DObj _, _
    | DList _, _
    | DTuple _, _
    | DResult _, _
    | DErrorRail _, _
    | DOption _, _
    | DStr _, _
    | DInt _, _
    | DDate _, _
    | DBool _, _
    | DFloat _, _
    | DNull, _
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

  let rec equalExpr (actual : Expr) (expected : Expr) (msg : string) : unit =
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
    dvalEquality (DObj m1) (DObj m2)

  // Raises a test exception if the two strings do not parse to identical JSON
  let equalsJson (str1 : string) (str2 : string) : unit =
    let parsed1 = Newtonsoft.Json.Linq.JToken.Parse str1
    let parsed2 = Newtonsoft.Json.Linq.JToken.Parse str2
    Expect.equal parsed1 parsed2 "jsons are equal"

let visitDval (f : Dval -> 'a) (dv : Dval) : List<'a> =
  let mutable state = []
  let f dv = state <- f dv :: state
  let rec visit dv : unit =
    match dv with
    // Keep for exhaustiveness checking
    | DObj map -> Map.values map |> List.map visit |> ignore<List<unit>>
    | DList dvs -> List.map visit dvs |> ignore<List<unit>>
    | DTuple (first, second, theRest) ->
      List.map visit ([ first; second ] @ theRest) |> ignore<List<unit>>
    | DHttpResponse (Response (_, _, v))
    | DResult (Error v)
    | DResult (Ok v)
    | DErrorRail v
    | DOption (Some v) -> visit v
    | DHttpResponse (Redirect _)
    | DOption None
    | DStr _
    | DInt _
    | DDate _
    | DBool _
    | DFloat _
    | DNull
    | DChar _
    | DPassword _
    | DFnVal _
    | DIncomplete _
    | DError _
    | DDB _
    | DUuid _
    | DBytes _
    | DStr _ -> f dv
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
  |> List.filterWithIndex (fun i (doc, str) ->
    i <> 139 && not (String.startsWith "#" str))

let interestingDvals =
  [ ("float", DFloat 7.2)
    ("float2", DFloat -7.2)
    ("float3", DFloat 15.0)
    ("float4", DFloat -15.0)
    ("int5", DInt 5L)
    ("true", DBool true)
    ("false", DBool false)
    ("null", DNull)
    ("datastore", DDB "Visitors")
    ("string", DStr "incredibly this was broken")
    // Json.NET has a habit of converting things automatically based on the type in the string
    ("date string", DStr "2018-09-14T00:31:41Z")
    ("int string", DStr "1039485")
    ("int string2", DStr "-1039485")
    ("int string3", DStr "0")
    ("uuid string", DStr "7d9e5495-b068-4364-a2cc-3633ab4d13e6")
    ("list", DList [ Dval.int 4 ])
    ("list with derror",
     DList [ Dval.int 3; DError(SourceNone, "some error string"); Dval.int 4 ])
    ("obj", DObj(Map.ofList [ "foo", Dval.int 5 ]))
    ("obj2", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DNull) ]))
    ("obj3", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DStr "x") ]))
    // More Json.NET tests
    ("obj4", DObj(Map.ofList [ "foo\\\\bar", Dval.int 5 ]))
    ("obj5", DObj(Map.ofList [ "$type", Dval.int 5 ]))
    ("obj with error",
     DObj(Map.ofList [ "v", DError(SourceNone, "some error string") ]))
    ("incomplete", DIncomplete SourceNone)
    ("incomplete2", DIncomplete(SourceID(14219007199254740993UL, 8UL)))
    ("error", DError(SourceNone, "some error string"))
    ("block",
     DFnVal(
       Lambda
         { body = RT.EBlank(id 1234)
           symtable = Map.empty
           parameters = [ (id 5678, "a") ] }
     ))
    ("block with pipe",
     DFnVal(
       Lambda
         { body =
             EApply(
               92356985UL,
               (EFQFnValue(
                 700731989UL,
                 FQFnName.Stdlib
                   { module_ = "List"; function_ = "push"; version = 0 }
               )),
               [ EApply(
                   93459985UL,
                   (EFQFnValue(
                     707841989UL,
                     FQFnName.Stdlib { module_ = ""; function_ = "+"; version = 0 }
                   )),
                   [ EApply(
                       394567785UL,
                       (EFQFnValue(
                         700766785UL,
                         FQFnName.Stdlib
                           { module_ = ""; function_ = "+"; version = 0 }
                       )),
                       [ EApply(
                           44444485UL,
                           (EFQFnValue(
                             893346989UL,
                             FQFnName.Stdlib
                               { module_ = ""; function_ = "+"; version = 0 }
                           )),
                           [ EInteger(234213618UL, 5); EInteger(923423468UL, 6) ],
                           NotInPipe,
                           NoRail
                         )
                         EInteger(648327618UL, 7) ],
                       InPipe(312312798UL),
                       NoRail
                     )
                     EInteger(325843618UL, 8) ],
                   NotInPipe,
                   NoRail
                 ) ],
               NotInPipe,
               NoRail
             )
           symtable = Map.empty
           parameters = [ (id 5678, "a") ] }
     ))
    ("errorrail", DErrorRail(Dval.int 5))
    ("errorrail with float",
     DErrorRail(DObj(Map.ofList ([ ("", DFloat nan); ("", DNull) ]))))
    ("redirect", DHttpResponse(Redirect "/home"))
    ("httpresponse",
     DHttpResponse(Response(200L, [ "content-length", "9" ], DStr "success")))
    ("db", DDB "Visitors")
    ("date",
     DDate(
       DDateTime.fromInstant (NodaTime.Instant.ofIsoString "2018-09-14T00:31:41Z")
     ))
    ("password", DPassword(Password(UTF8.toBytes "somebytes")))
    ("uuid", DUuid(System.Guid.Parse "7d9e5495-b068-4364-a2cc-3633ab4d13e6"))
    ("uuid0", DUuid(System.Guid.Parse "00000000-0000-0000-0000-000000000000"))
    ("option", DOption None)
    ("option2", DOption(Some(Dval.int 15)))
    ("option3", DOption(Some(DStr "a string")))
    ("character", DChar "s")
    ("result", DResult(Ok(Dval.int 15)))
    ("result2", DResult(Error(DList [ DStr "dunno if really supported" ])))
    ("result3", DResult(Ok(DStr "a string")))
    ("bytes", "JyIoXCg=" |> System.Convert.FromBase64String |> DBytes)
    // use image bytes here to test for any weird bytes forms
    ("bytes2",
     DBytes(
       LibBackend.File.readfileBytes
         LibBackend.Config.Testdata
         "sample_image_bytes.png"
     // TODO: deeply nested data
     ))

    ("simple2Tuple", DTuple(Dval.int 1, Dval.int 2, []))
    ("simple3Tuple", DTuple(Dval.int 1, Dval.int 2, [ Dval.int 3 ]))
    ("tupleWithNull", DTuple(Dval.int 1, Dval.int 2, [ DNull ]))
    ("tupleWithError", DTuple(Dval.int 1, DResult(Error(DStr "error")), [])) ]

let sampleDvals : List<string * Dval> =
  List.map (Tuple2.mapSecond DInt) interestingInts
  @ List.map (Tuple2.mapSecond DFloat) interestingFloats
    @ List.map (Tuple2.mapSecond DStr) interestingStrings
      @ List.map (Tuple2.mapSecond DStr) naughtyStrings @ interestingDvals

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
    .Services
    .AddLogging(fun loggingBuilder ->
      loggingBuilder.AddFile($"{LibBackend.Config.logDir}{name}.log", append = false)
      |> ignore<ILoggingBuilder>)
  |> ignore<IServiceCollection>
