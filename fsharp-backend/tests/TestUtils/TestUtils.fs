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
module S = LibExecution.Shortcuts

let testOwner : Lazy<Task<Account.UserInfo>> =
  lazy (UserName.create "test" |> Account.getUser |> Task.map Option.unwrapUnsafe)

let testAdmin : Lazy<Task<Account.UserInfo>> =
  lazy (UserName.create "dark" |> Account.getUser |> Task.map Option.unwrapUnsafe)

let testCanvasInfo (owner : Account.UserInfo) (name : string) : Task<Canvas.Meta> =
  task {
    let name = CanvasName.create name
    let! id = Canvas.canvasIDForCanvasName owner.id name
    return { id = id; name = name; owner = owner.id }
  }

let testCanvasID : Lazy<Task<CanvasID>> =
  lazy
    (task {
      let! owner = (testOwner.Force())
      let! canvasInfo = testCanvasInfo owner "test"
      return canvasInfo.id
    })

// delete test data for one canvas
let clearCanvasData (owner : Account.UserInfo) (name : CanvasName.T) : Task<unit> =
  task {
    let! canvasID = Canvas.canvasIDForCanvasName owner.id name

    let events =
      Sql.query "DELETE FROM events where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let storedEvents =
      Sql.query "DELETE FROM stored_events_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let functionResults =
      Sql.query "DELETE FROM function_results_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let functionArguments =
      Sql.query "DELETE FROM function_arguments where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let userData =
      Sql.query "DELETE FROM user_data where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let cronRecords =
      Sql.query "DELETE FROM cron_records where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let toplevelOplists =
      Sql.query "DELETE FROM toplevel_oplists where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    do!
      Task.WhenAll [| cronRecords
                      toplevelOplists
                      userData
                      functionArguments
                      functionResults
                      storedEvents
                      events |]

    do!
      Sql.query "DELETE FROM canvases where id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    return ()
  }

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
  (body : RT.Expr)
  : RT.UserFunction.T =
  { tlid = gid ()
    body = body
    description = ""
    infix = false
    name = name
    returnType = RT.TVariable "a"
    parameters =
      List.map
        (fun (p : string) ->
          { name = p; typ = RT.TVariable "b"; description = "test" })
        parameters }

let hop (h : PT.Handler.T) = PT.SetHandler(h.tlid, h.pos, h)

let libraries : Lazy<RT.Libraries> =
  lazy
    ({ stdlib =
         (LibExecutionStdLib.StdLib.fns @ BackendOnlyStdLib.StdLib.fns @ LibTest.fns
          |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name))

       packageFns = Map.empty })

let executionStateFor
  (owner : Account.UserInfo)
  (name : string)
  (dbs : Map<string, RT.DB.T>)
  (userFunctions : Map<string, RT.UserFunction.T>)
  : Task<RT.ExecutionState> =
  task {
    let ownerID : UserID = (owner : Account.UserInfo).id
    let executionID = ExecutionID $"test-{name}"

    // Performance optimization: don't touch the DB if you don't use the DB
    let hash =
      (sha1digest name |> System.Convert.ToBase64String |> String.toLowercase)
        .Replace("/", "")
        .Replace("=", "")
        .Replace("+", "")

    let canvasName = CanvasName.create $"test-{hash}"

    let! canvasID =
      if Map.count dbs > 0 then
        task {
          do! clearCanvasData owner canvasName
          return! Canvas.canvasIDForCanvasName ownerID canvasName
        }
      else
        task { return! testCanvasID.Force() }

    let tlid = id 7

    let program : RT.ProgramContext =
      { canvasID = canvasID
        canvasName = canvasName
        accountID = ownerID
        userFns = userFunctions
        dbs = dbs
        userTypes = Map.empty
        secrets = [] }

    let reportException (executionID : ExecutionID) (msg : string) (exn : exn) tags =
      // Don't rethrow exceptions as sometimes we want to test errors
      print "Exception Thrown"
      print exn.Message
      print exn.StackTrace

    return
      Exe.createState
        executionID
        (Lazy.force libraries)
        (Exe.noTracing RT.Real)
        reportException
        tlid
        program

  }

// saves and reloads the canvas for the Toplevvel
let canvasForTLs (meta : Canvas.Meta) (tls : List<PT.Toplevel>) : Task<Canvas.T> =
  task {
    let descs =
      tls
      |> List.map (fun tl ->
        let tlid = tl.toTLID ()

        let op =
          match tl with
          | PT.TLHandler h -> PT.SetHandler(h.tlid, { x = 0; y = 0 }, h)
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

// Many OCaml errors use a bunch of different fields, which seemed smart at the
// time but ultimately was pretty annoying. We can normalize by fetching the
// "short" field (there are other fields but we'll ignore them)
type OCamlError = { short : string }

let parseOCamlError (str : string) : string =
  try
    (Json.Vanilla.deserialize<OCamlError> str).short
  with
  | _ -> str

// Remove random things like IDs to make the tests stable
let normalizeDvalResult (dv : RT.Dval) : RT.Dval =
  match dv with
  | RT.DError (_, str) -> RT.DError(RT.SourceNone, parseOCamlError str)
  | RT.DIncomplete _ -> RT.DIncomplete(RT.SourceNone)
  | dv -> dv

open LibExecution.RuntimeTypes

let rec debugDval (v : Dval) : string =
  match v with
  | DStr s ->
    $"DStr '{s}'(len {s.Length}, {System.BitConverter.ToString(UTF8.toBytes s)})"
  | DDate d -> $"DDate '{d.toIsoString ()}': (millies {d.Millisecond})"
  | DObj obj ->
    obj
    |> Map.toList
    |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
    |> String.concat ",\n  "
    |> fun contents -> $"DObj {{\n  {contents}}}"

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
    | DObj vs -> vs |> Map.values |> List.all check
    | DStr str -> str.IsNormalized()
    | DChar str -> str.IsNormalized() && String.lengthInEgcs str = 1

  type Path = string list

  let pathToString (path : Path) : string =
    let pathStr = (path @ [ "val" ]) |> List.reverse |> String.concat "."
    $"in ({pathStr})"

  let rec patternEqualityBaseFn
    (checkIDs : bool)
    (path : Path)
    (actual : Pattern)
    (expected : Pattern)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    let eq path a e = patternEqualityBaseFn checkIDs path a e errorFn

    let check path (a : 'a) (e : 'a) =
      if a <> e then errorFn path (string actual) (string expected)

    let eqList path (l1 : List<RT.Pattern>) (l2 : List<RT.Pattern>) =
      List.iteri2 (fun i l r -> eq (string i :: path) l r) l1 l2
      check path (List.length l1) (List.length l2)

    if checkIDs then check path (Pattern.toID actual) (Pattern.toID expected)

    match actual, expected with
    | PVariable (_, name), PVariable (_, name') -> check path name name'
    | (PConstructor (_, name, patterns), PConstructor (_, name', patterns')) ->
      check path name name'
      eqList (name :: path) patterns patterns'
    | PString (_, str), PString (_, str') -> check path str str'
    | PInteger (_, l), PInteger (_, l') -> check path l l'
    | PFloat (_, d), PFloat (_, d') -> check path d d'
    | PBool (_, l), PBool (_, l') -> check path l l'
    | PCharacter (_, c), PCharacter (_, c') -> check path c c'
    | PNull (_), PNull (_) -> ()
    | PBlank (_), PBlank (_) -> ()
    // exhaustiveness check
    | PVariable _, _
    | PConstructor _, _
    | PString _, _
    | PInteger _, _
    | PFloat _, _
    | PBool _, _
    | PCharacter _, _
    | PNull _, _
    | PBlank _, _ -> check path actual expected


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
    | EPartial (_, e), EPartial (_, e') -> eq ("partial" :: path) e e'
    | ELambda (_, vars, e), ELambda (_, vars', e') ->
      let path = ("lambda" :: path)
      eq path e e'
      List.iteri2 (fun i (_, v) (_, v') -> check (string i :: path) v v') vars vars'
    | EMatch (_, e, branches), EMatch (_, e', branches') ->
      eq ("matchCond" :: path) e e'

      List.iter2
        (fun ((p, v) : Pattern * Expr) (p', v') ->
          patternEqualityBaseFn checkIDs path p p' errorFn
          eq (string p :: path) v v')
        branches
        branches'
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
    | EFQFnValue _, _
    | EApply _, _
    | ERecord _, _
    | EFieldAccess _, _
    | EFeatureFlag _, _
    | EConstructor _, _
    | EPartial _, _
    | ELambda _, _
    | EMatch _, _ -> check path actual expected



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
      check ("lamdaVars" :: path) (vals l1.parameters) (vals l2.parameters)
      check ("symbtable" :: path) l1.symtable l2.symtable // TODO: use dvalEquality
      exprEqualityBaseFn false path l1.body l2.body errorFn
    | DStr _, DStr _ -> check path (debugDval actual) (debugDval expected)
    // Keep for exhaustiveness checking
    | DHttpResponse _, _
    | DObj _, _
    | DList _, _
    | DResult _, _
    | DErrorRail _, _
    | DOption _, _
    | DStr _, _
    // All others can be directly compared
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
      Expect.equal a e $"{msg}: {pathToString path}")

  let rec equalPattern
    (actual : Pattern)
    (expected : Pattern)
    (msg : string)
    : unit =
    patternEqualityBaseFn true [] actual expected (fun path a e ->
      Expect.equal a e $"{msg}: {pathToString path}")

  let rec equalPatternIgnoringIDs (actual : Pattern) (expected : Pattern) : unit =
    patternEqualityBaseFn false [] actual expected (fun path a e ->
      Expect.equal a e (pathToString path))

  let rec equalExpr (actual : Expr) (expected : Expr) (msg : string) : unit =
    exprEqualityBaseFn true [] actual expected (fun path a e ->
      Expect.equal a e $"{msg}: {pathToString path}")

  let rec equalExprIgnoringIDs (actual : Expr) (expected : Expr) : unit =
    exprEqualityBaseFn false [] actual expected (fun path a e ->
      Expect.equal a e (pathToString path))

let dvalEquality (left : Dval) (right : Dval) : bool =
  let success = ref true
  Expect.dvalEqualityBaseFn [] left right (fun _ _ _ -> success.Value <- false)
  success.Value

let dvalMapEquality (m1 : DvalMap) (m2 : DvalMap) = dvalEquality (DObj m1) (DObj m2)

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
    [ ($"{doc} - 1", v - 1.0); ($"{doc} + 0", v); ($"{doc} + 1", v + 1.0) ])

let interestingInts : List<string * int64> =
  [ ("int0", 0L)
    ("int1", 1L)
    ("int-1", -1L)
    ("int_max_31_bits", 1073741824L)
    ("int_above_31_bits", 1073741825L)
    ("int_max_32_bits", 2147483647L)
    ("int_above_32_bits", 2147483648L)
    ("int_max_53_bits", 4503599627370496L)
    ("int_above_53_bits", 4503599627370497L)
    ("int_max_63_bits", 4611686018427387903L) ]
  |> List.flatMap (fun (doc, v) ->
    [ ($"{doc} - 1", v - 1L); ($"{doc} + 0", v); ($"{doc} + 1", v + 1L) ])

let sampleDvals : List<string * Dval> =
  (List.map (fun (doc, i) -> ($"int {doc}", DInt i)) interestingInts)
  @ (List.map (fun (doc, f) -> ($"float {doc}", DFloat f)) interestingFloats)
    @ [ ("float", DFloat 7.2)
        ("float2", DFloat -7.2)
        ("float3", DFloat 15.0)
        ("float4", DFloat -15.0)
        ("int5", RT.DInt 5L)
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
        ("obj", DObj(Map.ofList [ "foo", Dval.int 5 ]))
        ("obj2", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DNull) ]))
        ("obj3", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DStr "x") ]))
        // More Json.NET tests
        ("obj4", DObj(Map.ofList [ "foo\\\\bar", Dval.int 5 ]))
        ("obj5", DObj(Map.ofList [ "$type", Dval.int 5 ]))
        ("incomplete", DIncomplete SourceNone)
        ("error", DError(SourceNone, "some error string"))
        ("block",
         DFnVal(
           Lambda
             { body = RT.EBlank(id 1234)
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
        ("date", DDate(System.DateTime.ofIsoString "2018-09-14T00:31:41Z"))
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
         )) ]

// FSTODO: deeply nested data

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
