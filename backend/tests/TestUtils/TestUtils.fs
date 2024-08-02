// CLEANUP: split this file into smaller files
module TestUtils.TestUtils

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

// open Npgsql.FSharp
// open Npgsql
// open LibCloud.Db

open Prelude

module DarkDateTime = LibExecution.DarkDateTime
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module D = LibExecution.DvalDecoder
module PackageIDs = LibExecution.PackageIDs
module Exe = LibExecution.Execution

// module Account = LibCloud.Account
// module Canvas = LibCloud.Canvas
module S = RTShortcuts

module PackageIDs = LibExecution.PackageIDs
// module C2DT = LibExecution.CommonToDarkTypes
// module PT2DT = LibExecution.ProgramTypesToDarkTypes

//let pmPT = LibCloud.PackageManager.pt

//let testOwner : Lazy<Task<UserID>> = lazy (Account.createUser ())

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

// let initializeCanvasForOwner
//   (ownerID : UserID)
//   (name : string)
//   : Task<CanvasID * string> =
//   task {
//     let domain = nameToTestDomain name
//     let! canvasID = Canvas.create ownerID domain
//     return (canvasID, domain)
//   }

// let initializeTestCanvas' (name : string) : Task<CanvasID * string> =
//   task {
//     let! owner = testOwner.Force()
//     return! initializeCanvasForOwner owner name
//   }

// let initializeTestCanvas (name : string) : Task<CanvasID> =
//   task {
//     let! (canvasID, _domain) = initializeTestCanvas' name
//     return canvasID
//   }


// let testHttpRouteHandler
//   (route : string)
//   (method : string)
//   (ast : PT.Expr)
//   : PT.Handler.T =
//   { tlid = gid (); ast = ast; spec = PT.Handler.HTTP(route, method) }

// let testCron
//   (name : string)
//   (interval : PT.Handler.CronInterval)
//   (ast : PT.Expr)
//   : PT.Handler.T =
//   { tlid = gid (); ast = ast; spec = PT.Handler.Cron(name, interval) }

// let testWorker (name : string) (ast : PT.Expr) : PT.Handler.T =
//   { tlid = gid (); ast = ast; spec = PT.Handler.Worker name }

// let testPackageFn
//   (owner : string)
//   (name : string)
//   (typeParams : List<string>)
//   (parameters : NEList<string>)
//   (returnType : PT.TypeReference)
//   (body : PT.Expr)
//   : PT.PackageFn.PackageFn =
//   { id = System.Guid.NewGuid()
//     body = body
//     description = ""
//     name = PT.PackageFn.name owner [] name
//     typeParams = typeParams
//     deprecated = PT.NotDeprecated
//     parameters =
//       NEList.map
//         (fun p -> { name = p; typ = PT.TVariable "b"; description = "test" })
//         parameters
//     returnType = returnType }



// let testDB (name : string) (typ : PT.TypeReference) : PT.DB.T =
//   { tlid = gid (); name = name; typ = typ; version = 0 }



let builtins
  //(httpConfig : BuiltinExecution.Libs.HttpClient.Configuration)
  (_pm : PT.PackageManager)
  : RT.Builtins =
  LibExecution.Builtin.combine
    [ LibTest.builtins
      BuiltinExecution.Builtin.builtins //httpConfig pm
      // BuiltinCloudExecution.Builtin.builtins
      // BuiltinDarkInternal.Builtin.builtins
      // BuiltinCli.Builtin.builtins
      ]
    []

// let cloudBuiltIns (pm : PT.PackageManager) =
//   let httpConfig =
//     { LibCloudExecution.HttpClient.configuration with
//         timeoutInMs = 5000
//         allowedIP = (fun _ -> true) }
//   builtins httpConfig pm

let localBuiltIns (pm : PT.PackageManager) =
  // let httpConfig =
  //   { BuiltinExecution.Libs.HttpClient.defaultConfig with timeoutInMs = 5000 }
  // builtins httpConfig pm
  builtins pm



let executionStateFor
  (pmPT : PT.PackageManager)
  (canvasID : CanvasID)
  (internalFnsAllowed : bool)
  (_allowLocalHttpAccess : bool)
  //(dbs : Map<string, RT.DB.T>)
  : Task<RT.ExecutionState> =
  task {
    let domains = [] //Canvas.domainsForCanvasID canvasID

    let program : RT.Program =
      { canvasID = canvasID
        internalFnsAllowed = internalFnsAllowed
      // dbs = dbs
      // secrets = []
      }

    let testContext : RT.TestContext =
      { sideEffectCount = 0
        exceptionReports = []
        expectedExceptionCount = 0
        postTestExecutionHook =
          fun tc ->
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

    let builtins =
      //if allowLocalHttpAccess then localBuiltIns pmPT else cloudBuiltIns pmPT
      localBuiltIns pmPT
    let state =
      let pmRT = PT2RT.PackageManager.toRT pmPT
      let tracing = Exe.noTracing (RT.CallStack.fromEntryPoint RT.Script)
      Exe.createState builtins pmRT tracing exceptionReporter notifier program
    let state = { state with test = testContext }
    return state
  }

// /// Saves and reloads the canvas for the Toplevels
// let canvasForTLs (canvasID : CanvasID) (tls : List<PT.Toplevel.T>) : Task<Canvas.T> =
//   task {
//     let descs = tls |> List.map (fun tl -> (tl, LibCloud.Serialize.NotDeleted))
//     do! Canvas.saveTLIDs canvasID descs
//     return! Canvas.loadAll canvasID
//   }



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

let testManyPly (name : string) (fn : 'a -> Ply<'b>) (values : List<'a * 'b>) =
  testManyTask name (fn >> Ply.toTask) values


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
  // Nothing interesting right now
  match dv with
  | dv -> dv

open LibExecution.RuntimeTypes

let rec debugDval (v : Dval) : string =
  match v with
  // most Dvals print out reasonably nice-looking values automatically,
  // but in these cases we'd like to print something a bit prettier

  | DString s ->
    $"DString '{s}'(len {s.Length}, {System.BitConverter.ToString(UTF8.toBytes s)})"

  // | DDateTime d ->
  //   $"DDateTime '{DarkDateTime.toIsoString d}': (millies {d.InUtc().Millisecond})"

  // | DRecord(tn, _, typeArgs, o) ->
  //   let typeStr = FQTypeName.toString tn

  //   let typeArgsPart =
  //     match typeArgs with
  //     | [] -> ""
  //     | _ ->
  //       typeArgs
  //       |> List.map ValueType.toString
  //       |> String.concat ", "
  //       |> fun s -> $"<{s}>"

  //   let fieldsPart =
  //     o
  //     |> Map.toList
  //     |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
  //     |> String.concat ",\n  "

  //   $"DRecord {typeStr}{typeArgsPart} {{\n  {fieldsPart}}}"

  // | DDict(_vtTODO, obj) ->
  //   obj
  //   |> Map.toList
  //   |> List.map (fun (k, v) -> $"\"{k}\": {debugDval v}")
  //   |> String.concat ",\n  "
  //   |> fun contents -> $"DDict {{\n  {contents}}}"

  | _ -> v.ToString()

module Expect =
  // Checks if the value (and all its contents) is in its desired
  // representation (in the event that there are multiple ways to represent
  // it). Think of this as a general form of string normalization.
  let rec isCanonical (dv : Dval) : bool =
    let r = isCanonical

    match dv with
    | DUnit
    | DBool _

    | DInt8 _
    | DUInt8 _
    | DInt16 _
    | DUInt16 _
    | DInt32 _
    | DUInt32 _
    | DInt64 _
    | DUInt64 _
    | DInt128 _
    | DUInt128 _

    | DFloat _

    | DDateTime _
    | DUuid _
    | DFnVal _
    // | DDB _
     -> true

    | DChar str -> str.IsNormalized() && String.lengthInEgcs str = 1
    | DString str -> str.IsNormalized()

    | DList(_, items) -> List.all r items
    // | DTuple(first, second, rest) -> List.all r ([ first; second ] @ rest)
    | DDict(_, entries) -> entries |> Map.values |> List.all r

  // | DRecord(_, _, _, fields) -> fields |> Map.values |> List.all r
  // | DEnum(_, _, _, _, fields) -> fields |> List.all r

  type Path = string list

  let pathToString (path : Path) : string =
    if path = [] then
      ""
    else
      let path = path @ [ "value" ] |> List.reverse |> String.concat "."
      $" `{path}` of"

  // let rec letPatternEqualityBaseFn
  //   (checkIDs : bool)
  //   (path : Path)
  //   (actual : LetPattern)
  //   (expected : LetPattern)
  //   (errorFn : Path -> string -> string -> unit)
  //   : unit =
  //   let check path (a : 'a) (e : 'a) =
  //     if a <> e then errorFn path (string actual) (string expected)

  //   if checkIDs then check path (LetPattern.toID actual) (LetPattern.toID expected)

  //   match actual, expected with
  //   | LPVariable(_, name), LPVariable(_, name') -> check path name name'
  //   | LPUnit(_), LPUnit(_) -> ()
  //   | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
  //     let all = first :: second :: theRest
  //     let all' = first' :: second' :: theRest'
  //     let zipped = List.zip all all'
  //     List.iter
  //       (fun (a, e) ->
  //         letPatternEqualityBaseFn checkIDs (path @ [ "tuple" ]) a e errorFn)
  //       zipped

  //   // exhaustive match
  //   | LPVariable _, _
  //   | LPUnit _, _
  //   | LPTuple _, _ -> errorFn path (string actual) (string expected)


  // let rec userTypeNameEqualityBaseFn
  //   (path : Path)
  //   (actual : FQTypeName.FQTypeName)
  //   (expected : FQTypeName.FQTypeName)
  //   (errorFn : Path -> string -> string -> unit)
  //   : unit =
  //   let err () = errorFn path (string actual) (string expected)

  //   match actual, expected with
  //   | FQTypeName.Package a, FQTypeName.Package e -> if a <> e then err ()

  // let rec matchPatternEqualityBaseFn
  //   (checkIDs : bool)
  //   (path : Path)
  //   (actual : MatchPattern)
  //   (expected : MatchPattern)
  //   (errorFn : Path -> string -> string -> unit)
  //   : unit =
  //   let eq path a e = matchPatternEqualityBaseFn checkIDs path a e errorFn

  //   let check path (a : 'a) (e : 'a) =
  //     if a <> e then errorFn path (string actual) (string expected)

  //   let eqList path (l1 : List<RT.MatchPattern>) (l2 : List<RT.MatchPattern>) =
  //     List.iteri2 (fun i -> eq (string i :: path)) l1 l2
  //     check path (List.length l1) (List.length l2)

  //   if checkIDs then
  //     check path (MatchPattern.toID actual) (MatchPattern.toID expected)

  //   match actual, expected with
  //   | MPVariable(_, name), MPVariable(_, name') -> check path name name'
  //   | (MPEnum(_, caseName, fieldPats), MPEnum(_, caseName', fieldPats')) ->
  //     check path caseName caseName'
  //     eqList (caseName :: path) fieldPats fieldPats'
  //   | MPString(_, str), MPString(_, str') -> check path str str'
  //   | MPInt64(_, l), MPInt64(_, l') -> check path l l'
  //   | MPUInt64(_, l), MPUInt64(_, l') -> check path l l'
  //   | MPInt8(_, l), MPInt8(_, l') -> check path l l'
  //   | MPUInt8(_, l), MPUInt8(_, l') -> check path l l'
  //   | MPInt16(_, l), MPInt16(_, l') -> check path l l'
  //   | MPUInt16(_, l), MPUInt16(_, l') -> check path l l'
  //   | MPInt32(_, l), MPInt32(_, l') -> check path l l'
  //   | MPUInt32(_, l), MPUInt32(_, l') -> check path l l'
  //   | MPInt128(_, l), MPInt128(_, l') -> check path l l'
  //   | MPUInt128(_, l), MPUInt128(_, l') -> check path l l'
  //   | MPFloat(_, d), MPFloat(_, d') -> check path d d'
  //   | MPBool(_, l), MPBool(_, l') -> check path l l'
  //   | MPChar(_, c), MPChar(_, c') -> check path c c'
  //   | MPUnit(_), MPUnit(_) -> ()
  //   | MPTuple(_, first, second, theRest), MPTuple(_, first', second', theRest') ->
  //     eqList path (first :: second :: theRest) (first' :: second' :: theRest')
  //   | MPList(_, pats), MPList(_, pats') -> eqList path pats pats'
  //   | MPListCons(_, head, tail), MPListCons(_, head', tail') ->
  //     check path head head'
  //     check path tail tail'
  //   // exhaustiveness check
  //   | MPVariable _, _
  //   | MPEnum _, _
  //   | MPString _, _
  //   | MPInt64 _, _
  //   | MPUInt64 _, _
  //   | MPInt8 _, _
  //   | MPUInt8 _, _
  //   | MPInt16 _, _
  //   | MPUInt16 _, _
  //   | MPInt32 _, _
  //   | MPUInt32 _, _
  //   | MPInt128 _, _
  //   | MPUInt128 _, _
  //   | MPFloat _, _
  //   | MPBool _, _
  //   | MPChar _, _
  //   | MPUnit _, _
  //   | MPTuple _, _
  //   | MPListCons _, _
  //   | MPList _, _ -> check path actual expected



  let dTypeEqualityBaseFn
    (path : Path)
    (actual : TypeReference)
    (expected : TypeReference)
    (errorFn : Path -> string -> string -> unit)
    : unit =
    // as long as TypeReferences don't get IDs, depending on structural equality is OK
    if actual <> expected then errorFn path (string actual) (string expected)



  // let rec exprEqualityBaseFn
  //   (checkIDs : bool)
  //   (path : Path)
  //   (actual : Expr)
  //   (expected : Expr)
  //   (errorFn : Path -> string -> string -> unit)
  //   : unit =
  //   let eq path a e = exprEqualityBaseFn checkIDs path a e errorFn

  //   let check path (a : 'a) (e : 'a) =
  //     if a <> e then errorFn path (string actual) (string expected)

  //   let eqList path (l1 : List<RT.Expr>) (l2 : List<RT.Expr>) =
  //     List.iteri2 (fun i -> eq (string i :: path)) l1 l2
  //     check path (List.length l1) (List.length l2)

  //   let eqNEList path (l1 : NEList<RT.Expr>) (l2 : NEList<RT.Expr>) =
  //     NEList.iteri2 (fun i -> eq (string i :: path)) l1 l2
  //     check path (NEList.length l1) (NEList.length l2)

  //   if checkIDs then check path (Expr.toID actual) (Expr.toID expected)

  //   match actual, expected with
  //   // expressions with no values
  //   | EUnit _, EUnit _ -> ()


  //   // Simple exprs
  //   | EBool(_, v), EBool(_, v') -> check path v v'

  //   // | EInt8(_, v), EInt8(_, v') -> check path v v'
  //   // | EUInt8(_, v), EUInt8(_, v') -> check path v v'
  //   // | EInt16(_, v), EInt16(_, v') -> check path v v'
  //   // | EUInt16(_, v), EUInt16(_, v') -> check path v v'
  //   // | EInt32(_, v), EInt32(_, v') -> check path v v'
  //   // | EUInt32(_, v), EUInt32(_, v') -> check path v v'
  //   | EInt64(_, v), EInt64(_, v') -> check path v v'
  //   // | EUInt64(_, v), EUInt64(_, v') -> check path v v'
  //   // | EInt128(_, v), EInt128(_, v') -> check path v v'
  //   // | EUInt128(_, v), EUInt128(_, v') -> check path v v'

  //   // | EFloat(_, v), EFloat(_, v') -> check path v v'

  //   // expressions with single string values
  //   | EString(_, s), EString(_, s') ->
  //     let rec checkSegment s s' =
  //       match s, s' with
  //       | StringText s, StringText s' -> check path s s'
  //       | StringInterpolation e, StringInterpolation e' -> eq path e e'
  //       | _ -> check path s s'
  //     List.iter2 checkSegment s s'

  //   // | EChar(_, v), EChar(_, v')
  //   // | EVariable(_, v), EVariable(_, v') -> check path v v'
  //   // | EConstant(_, name), EConstant(_, name') -> check path name name'
  //   // | ELet(_, pat, rhs, body), ELet(_, pat', rhs', body') ->
  //   //   letPatternEqualityBaseFn checkIDs path pat pat' errorFn
  //   //   eq ("rhs" :: path) rhs rhs'
  //   //   eq ("body" :: path) body body'
  //   // | EIf(_, con, thn, els), EIf(_, con', thn', els') ->
  //   //   eq ("cond" :: path) con con'
  //   //   eq ("then" :: path) thn thn'
  //   //   match els, els' with
  //   //   | Some el, Some el' -> eq ("else" :: path) el el'
  //   //   | None, None -> ()
  //   //   | _ ->
  //   //     errorFn ("else" :: path) (string actual) (string expected)
  //   //     ()

  //   // | EList(_, l), EList(_, l') -> eqList path l l'
  //   // | ETuple(_, first, second, theRest), ETuple(_, first', second', theRest') ->
  //   //   eq ("first" :: path) first first'
  //   //   eq ("second" :: path) second second'
  //   //   eqList path theRest theRest'

  //   | EApply(_, name, typeArgs, args), EApply(_, name', typeArgs', args') ->
  //     let path = (string name :: path)
  //     eq path name name'

  //     check path (List.length typeArgs) (List.length typeArgs')
  //     List.iteri2
  //       (fun i l r -> dTypeEqualityBaseFn (string i :: path) l r errorFn)
  //       typeArgs
  //       typeArgs'

  //     eqNEList path args args'

  //   | EFnName(_, name), EFnName(_, name') -> check path name name'

  //   // | ERecord(_, typeName, fields), ERecord(_, typeName', fields') ->
  //   //   userTypeNameEqualityBaseFn path typeName typeName' errorFn
  //   //   NEList.iter2
  //   //     (fun (k, v) (k', v') ->
  //   //       check path k k'
  //   //       eq (k :: path) v v')
  //   //     fields
  //   //     fields'
  //   // | ERecordUpdate(_, record, updates), ERecordUpdate(_, record', updates') ->
  //   //   check path record record'
  //   //   NEList.iter2
  //   //     (fun (k, v) (k', v') ->
  //   //       check path k k'
  //   //       eq (k :: path) v v')
  //   //     updates
  //   //     updates'
  //   // | EDict(_, fields), EDict(_, fields') ->
  //   //   List.iter2
  //   //     (fun (k, v) (k', v') ->
  //   //       check ("key" :: path) k k'
  //   //       eq ("value" :: path) v v')
  //   //     fields
  //   //     fields'

  //   // | EFieldAccess(_, e, f), EFieldAccess(_, e', f') ->
  //   //   eq (f :: path) e e'
  //   //   check path f f'

  //   // | EEnum(_, typeName, caseName, fields), EEnum(_, typeName', caseName', fields') ->
  //   //   userTypeNameEqualityBaseFn path typeName typeName' errorFn
  //   //   check path caseName caseName'
  //   //   eqList path fields fields'
  //   //   ()

  //   // | ELambda(_, pats, e), ELambda(_, pats', e') ->
  //   //   let path = ("lambda" :: path)
  //   //   eq path e e'
  //   //   NEList.iter2
  //   //     (fun pat pat' -> letPatternEqualityBaseFn false path pat pat' errorFn)
  //   //     pats
  //   //     pats'
  //   // | EMatch(_, e, branches), EMatch(_, e', branches') ->
  //   //   eq ("matchCond" :: path) e e'

  //   //   check path (NEList.length branches) (NEList.length branches')
  //   //   NEList.iteri2
  //   //     (fun i branch branch' ->
  //   //       let path = $"Case {i} - {branch.pat}" :: path
  //   //       matchPatternEqualityBaseFn
  //   //         checkIDs
  //   //         ("pat" :: path)
  //   //         branch.pat
  //   //         branch'.pat
  //   //         errorFn
  //   //       match branch.whenCondition, branch'.whenCondition with
  //   //       | Some cond, Some cond' -> eq ("whenCondition" :: path) cond cond'
  //   //       | None, None -> ()
  //   //       | _ ->
  //   //         errorFn ("whenCondition" :: path) (string actual) (string expected)
  //   //         ()
  //   //       eq ("rhs" :: path) branch.rhs branch'.rhs)
  //   //     branches
  //   //     branches'
  //   // | EAnd(_, l, r), EAnd(_, l', r') ->
  //   //   eq ("left" :: path) l l'
  //   //   eq ("right" :: path) r r'
  //   // | EOr(_, l, r), EOr(_, l', r') ->
  //   //   eq ("left" :: path) l l'
  //   //   eq ("right" :: path) r r'
  //   | EError(_, msg, exprs), EError(_, msg', exprs') ->
  //     check path msg msg'
  //     eqList path exprs exprs'

  //   // exhaustiveness check
  //   | EUnit _, _
  //   // | EInt8 _, _
  //   // | EUInt8 _, _
  //   // | EInt16 _, _
  //   // | EUInt16 _, _
  //   // | EInt32 _, _
  //   // | EUInt32 _, _
  //   | EInt64 _, _
  //   // | EUInt64 _, _
  //   // | EInt128 _, _
  //   // | EUInt128 _, _
  //   | EString _, _
  //   // | EChar _, _
  //   // | EVariable _, _
  //   // | EConstant _, _
  //   | EBool _, _
  //   // | EFloat _, _
  //   // | ELet _, _
  //   // | EIf _, _
  //   // | EList _, _
  //   // | ETuple _, _
  //   | EApply _, _
  //   | EFnName _, _
  //   // | ERecord _, _
  //   // | ERecordUpdate _, _
  //   // | EDict _, _
  //   // | EFieldAccess _, _
  //   // | EEnum _, _
  //   // | ELambda _, _
  //   // | EMatch _, _
  //   // | EAnd _, _
  //   // | EOr _, _
  //   | EError _, _ -> check path actual expected



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

    let checkValueType (path : Path) (a : ValueType) (e : ValueType) : unit =
      match VT.merge a e with
      | Ok _merged -> ()
      | Error() -> errorFn path (debugDval actual) (debugDval expected)

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
      else if
        System.Double.IsNaN l
        || System.Double.IsNaN r
        || System.Double.IsPositiveInfinity l
        || System.Double.IsPositiveInfinity r
        || System.Double.IsNegativeInfinity l
        || System.Double.IsNegativeInfinity r
      then
        error path
      else if not (Accuracy.areClose Accuracy.veryHigh l r) then
        error path
    | DDateTime l, DDateTime r ->
      // Two dates can be the same millisecond and not be equal if they don't
      // have the same number of ticks. For testing, we shall consider them
      // equal if they print the same string.
      check path (string l) (string r)

    | DList(lType, ls), DList(rType, rs) ->
      checkValueType ("Type" :: path) lType rType

      check ("Length" :: path) (List.length ls) (List.length rs)
      List.iteri2 (fun i -> de ($"[{i}]" :: path)) ls rs

    // | DTuple(firstL, secondL, theRestL), DTuple(firstR, secondR, theRestR) ->
    //   de path firstL firstR

    //   de path secondL secondR

    //   check ("Length" :: path) (List.length theRestL) (List.length theRestR)
    //   List.iteri2 (fun i -> de ($"[{i}]" :: path)) theRestL theRestR

    | DDict(lType, ls), DDict(rType, rs) ->
      check ("Length" :: path) (Map.count ls) (Map.count rs)

      checkValueType ("Type" :: path) lType rType

      // check keys from ls are in both, check matching values
      Map.iterWithIndex
        (fun key v1 ->
          match Map.find key rs with
          | Some v2 -> de (key :: path) v1 v2
          | None -> check (key :: path) ls rs)
        ls

      // check keys from rs are in both
      Map.iterWithIndex
        (fun key _ ->
          match Map.find key rs with
          | Some _ -> () // already checked
          | None -> check (key :: path) ls rs)
        rs


    // | DRecord(ltn, _, ltypeArgs, ls), DRecord(rtn, _, rtypeArgs, rs) ->
    //   // check type name
    //   userTypeNameEqualityBaseFn path ltn rtn errorFn

    //   // check type args
    //   check
    //     ("TypeArgsLength" :: path)
    //     (List.length ltypeArgs)
    //     (List.length rtypeArgs)
    //   List.iteri2 (fun i -> checkValueType (string i :: path)) ltypeArgs rtypeArgs

    //   check ("Length" :: path) (Map.count ls) (Map.count rs)

    //   // check keys
    //   // -- keys from ls are in both, check matching values
    //   Map.iterWithIndex
    //     (fun key v1 ->
    //       match Map.find key rs with
    //       | Some v2 -> de (key :: path) v1 v2
    //       | None -> check (key :: path) ls rs)
    //     ls

    //   // -- keys from rs are in both
    //   Map.iterWithIndex
    //     (fun key _ ->
    //       match Map.find key rs with
    //       | Some _ -> () // already checked
    //       | None -> check (key :: path) ls rs)
    //     rs


    // | DEnum(typeName, _, typeArgs, caseName, fields),
    //   DEnum(typeName', _, typeArgs', caseName', fields') ->
    //   userTypeNameEqualityBaseFn path typeName typeName' errorFn
    //   check ("caseName" :: path) caseName caseName'

    //   check ("TypeArgsLength" :: path) (List.length typeArgs) (List.length typeArgs')
    //   List.iteri2 (fun i -> checkValueType (string i :: path)) typeArgs typeArgs'

    //   check ("fields.Length" :: path) (List.length fields) (List.length fields)
    //   List.iteri2 (fun i -> de ($"[{i}]" :: path)) fields fields'
    //   ()

    // | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
    //   NEList.iter2
    //     (fun pat pat' -> letPatternEqualityBaseFn false path pat pat' errorFn)
    //     l1.parameters
    //     l2.parameters
    //   check ("symbtable" :: path) l1.symtable l2.symtable // TODO: use dvalEquality
    //   exprEqualityBaseFn false path l1.body l2.body errorFn

    | DString _, DString _ -> check path (debugDval actual) (debugDval expected)

    // Keep for exhaustiveness checking
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
    // | DTuple _, _
    | DDict _, _
    // | DRecord _, _
    // | DEnum _, _
    | DFnVal _, _
    // | DDB _, _
     -> check path actual expected

  let formatMsg (initialMsg : string) (path : Path) (actual : 'a) : string =
    let initial = if initialMsg = "" then "" else $"{initialMsg}\n\n"
    $"{initial}Error was found in{pathToString path}:\nError was:\n{actual})\n\n"

  let rec equalDval (actual : Dval) (expected : Dval) (msg : string) : unit =
    dvalEqualityBaseFn [] actual expected (fun path a e ->
      Expect.equal a e (formatMsg msg path actual))

  // let rec equalMatchPattern
  //   (actual : MatchPattern)
  //   (expected : MatchPattern)
  //   (msg : string)
  //   : unit =
  //   matchPatternEqualityBaseFn true [] actual expected (fun path a e ->
  //     Expect.equal a e (formatMsg msg path actual))

  // let rec equalMatchPatternIgnoringIDs
  //   (actual : MatchPattern)
  //   (expected : MatchPattern)
  //   : unit =
  //   matchPatternEqualityBaseFn false [] actual expected (fun path a e ->
  //     Expect.equal a e (formatMsg "" path actual))

  // let rec equalExpr (actual : Expr) (expected : Expr) (msg : string) : unit =
  //   exprEqualityBaseFn true [] actual expected (fun path a e ->
  //     Expect.equal a e (formatMsg msg path actual))

  // let rec equalExprIgnoringIDs (actual : Expr) (expected : Expr) : unit =
  //   exprEqualityBaseFn false [] actual expected (fun path a e ->
  //     Expect.equal a e (formatMsg "" path actual))

  let dvalEquality (left : Dval) (right : Dval) : bool =
    let mutable success = true
    dvalEqualityBaseFn [] left right (fun _ _ _ -> success <- false)
    success

let visitDval (f : Dval -> 'a) (dv : Dval) : List<'a> =
  let mutable state = []
  let f dv = state <- f dv :: state
  let rec visit dv : unit =
    match dv with
    | DDict(_, entries) -> Map.values entries |> List.map visit |> ignore<List<unit>>
    // | DRecord(_, _, _, fields) ->
    //   Map.values fields |> List.map visit |> ignore<List<unit>>
    // | DEnum(_, _, _, _, fields) -> fields |> List.map visit |> ignore<List<unit>>
    | DList(_, items) -> List.map visit items |> ignore<List<unit>>
    // | DTuple(first, second, theRest) ->
    //   List.map visit ([ first; second ] @ theRest) |> ignore<List<unit>>

    // Keep for exhaustiveness checking
    | DUnit
    | DBool _
    | DInt8 _
    | DUInt8 _
    | DInt16 _
    | DUInt16 _
    | DInt32 _
    | DUInt32 _
    | DInt64 _
    | DUInt64 _
    | DInt128 _
    | DUInt128 _
    | DFloat _
    | DChar _
    | DString _ // TODO: should actually traverse in interpolations
    | DUuid _
    | DDateTime _
    | DFnVal _
    // | DDB _
     -> f dv
    f dv
  visit dv
  state


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



// let interestingFloats : List<string * float> =
//   let initial =
//     // interesting cause we used to use 31 bit ints
//     [ "min 31 bit", System.Math.Pow(2.0, 30.0) - 1.0
//       "max 31 bit", - System.Math.Pow(2.0, 30.0)
//       // interesting cause boundary of 32 bit ints
//       "min 32 bit", System.Math.Pow(2.0, 31.0) - 1.0
//       "max 32 bit", - System.Math.Pow(2.0, 31.0)
//       // interesting cause doubles support up to 53-bit ints
//       "min 53 bit", System.Math.Pow(2.0, 52.0) - 1.0
//       "max 53 bit", - System.Math.Pow(2.0, 52.0)
//       // interesting cause we used to have 63 bit ints
//       "min 63 bit", System.Math.Pow(2.0, 62.0) - 1.0
//       "max 63 bit", - System.Math.Pow(2.0, 62.0)
//       // interesting cause boundary of 64 bit ints
//       "min 64 bit", System.Math.Pow(2.0, 63.0) - 1.0
//       "max 64 bit", - System.Math.Pow(2.0, 63.0)
//       // Interesting anyway
//       "zero", 0.0
//       "negative zero", -0.0
//       "NaN", nan
//       "infinity", infinity
//       "-infinity", -infinity
//       // Mathy values
//       "e", System.Math.E
//       "pi", System.Math.PI
//       "tau", System.Math.Tau ]

//   initial
//   |> List.map (fun (doc, v) ->
//     [ ($"float {doc} - 1", v - 1.0); ($"{doc} + 0", v); ($"{doc} + 1", v + 1.0) ])
//   |> List.flatten

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
  |> List.map (fun (doc, v) ->
    [ ($"int {doc} - 1", v - 1L); ($"{doc} + 0", v); ($"{doc} + 1", v + 1L) ])
  |> List.flatten


// // https://github.com/minimaxir/big-list-of-naughty-strings
// let naughtyStrings : List<string * string> =
//   LibCloud.File.readfile LibCloud.Config.Testdata "naughty-strings.txt"
//   |> String.splitOnNewline
//   |> List.mapWithIndex (fun i s -> $"naughty string line {i + 1}", s)
//   // 139 is the Unicode BOM on line 140, which is tough to get .NET to put in a string
//   |> List.filterWithIndex (fun i (_, str) ->
//     i <> 139 && not (String.startsWith "#" str))


// let interestingDvals : List<string * RT.Dval * RT.TypeReference> =
//   let uuid = System.Guid.Parse "dca045b1-e2af-41d8-ad1b-35261b25a426"

//   [ ("float", DFloat 7.2, TFloat)
//     ("float2", DFloat -7.2, TFloat)
//     ("float3", DFloat 15.0, TFloat)
//     ("float4", DFloat -15.0, TFloat)
//     ("int5", DInt64 5L, TInt64)
//     ("int_8_bits", DInt8 127y, TInt8)
//     ("int_16_bits", DInt16 32767s, TInt16)
//     ("int_32_bits", DInt32 2147483647l, TInt32)
//     ("int_128_bits", DInt128 170141183460469231731687303715884105727Q, TInt128)
//     ("uint_8_bits", DUInt8 255uy, TUInt8)
//     ("uint_16_bits", DUInt16 65535us, TUInt16)
//     ("uint_32_bits", DUInt32 4294967295ul, TUInt32)
//     ("uint_64_bits", DUInt64 18446744073709551615UL, TUInt64)
//     ("uint_128_bits", DUInt128 340282366920938463463374607431768211455Z, TUInt128)
//     ("true", DBool true, TBool)
//     ("false", DBool false, TBool)
//     ("unit", DUnit, TUnit)
//     ("datastore", DDB "Visitors", TDB TInt64)
//     ("string", DString "incredibly this was broken", TString)
//     // Json.NET has a habit of converting things automatically based on the type in the string
//     ("date string", DString "2018-09-14T00:31:41Z", TString)
//     ("int string", DString "1039485", TString)
//     ("int string2", DString "-1039485", TString)
//     ("int string3", DString "0", TString)
//     ("uuid string", DString "7d9e5495-b068-4364-a2cc-3633ab4d13e6", TString)
//     ("list", DList(ValueType.Known KTInt64, [ Dval.int64 4 ]), TList TInt64)

//     ("record",
//      DRecord(
//        FQTypeName.Package uuid,
//        FQTypeName.Package uuid,
//        [],
//        Map.ofList
//          [ "url", DString "https://darklang.com"
//            "headers", Dval.list (KTTuple(VT.string, VT.string, [])) []
//            "body", Dval.list KTUInt8 [] ]
//      ),
//      TCustomType(Ok(FQTypeName.Package uuid), []))

//     ("enum",
//      DEnum(
//        FQTypeName.Package PackageIDs.Type.Stdlib.AltJson.json,
//        FQTypeName.Package PackageIDs.Type.Stdlib.AltJson.json,
//        [],
//        "String",
//        [ DString "test" ]
//      ),
//      TCustomType(Ok(FQTypeName.Package PackageIDs.Type.Stdlib.AltJson.json), []))

//     // TODO: extract what's useful in here, and create smaller tests for each
//     ("record2",
//      DRecord(
//        FQTypeName.Package uuid,
//        FQTypeName.Package uuid,
//        [ VT.unknown; VT.bool ],
//        Map.ofList [ ("type", DString "weird"); ("value", DUnit) ]
//      ),
//      TCustomType(Ok(FQTypeName.Package uuid), []))
//     ("record3",
//      DRecord(
//        FQTypeName.Package uuid,
//        FQTypeName.Package uuid,
//        [],
//        Map.ofList [ ("type", DString "weird"); ("value", DString "x") ]
//      ),
//      TCustomType(Ok(FQTypeName.Package uuid), []))
//     // More Json.NET tests
//     ("record4",
//      DRecord(
//        FQTypeName.Package uuid,
//        FQTypeName.Package uuid,
//        [ VT.bool; VT.char; (VT.customType (FQTypeName.Package uuid)) [] ],
//        Map.ofList [ "foo\\\\bar", Dval.int64 5 ]
//      ),
//      TCustomType(Ok(FQTypeName.Package uuid), []))
//     ("record5",
//      DRecord(
//        FQTypeName.Package uuid,
//        FQTypeName.Package uuid,
//        [],
//        Map.ofList [ "$type", Dval.int64 5 ]
//      ),
//      TCustomType(Ok(FQTypeName.Package uuid), []))
//     ("dict", DDict(VT.unknown, Map [ "foo", Dval.int64 5 ]), TDict TInt64)
//     ("dict3",
//      DDict(VT.unknown, Map [ ("type", DString "weird"); ("value", DString "x") ]),
//      TDict TString)
//     // More Json.NET tests
//     ("dict4", DDict(VT.unknown, Map [ "foo\\\\bar", Dval.int64 5 ]), TDict TInt64)
//     ("dict5", DDict(VT.unknown, Map [ "$type", Dval.int64 5 ]), TDict TInt64)
//     ("lambda",
//      DFnVal(
//        Lambda
//          { body = RT.EUnit(id 1234)
//            typeSymbolTable = Map.empty
//            symtable = Map.empty
//            parameters = NEList.singleton (RT.LPVariable(id 5678, "a")) }
//      ),
//      TFn(NEList.singleton TInt64, TUnit))
//     ("lambda with pipe",
//      DFnVal(
//        Lambda
//          { body =
//              EApply(
//                92356985UL,
//                (EFnName(
//                  957274UL,
//                  FQFnName.Builtin { name = "listPush"; version = 0 }
//                )),
//                [],
//                NEList.singleton (
//                  EApply(
//                    93459985UL,
//                    (EFnName(123123UL, FQFnName.Builtin { name = "+"; version = 0 })),
//                    [],
//                    (NEList.doubleton
//                      (EApply(
//                        394567785UL,
//                        (EFnName(
//                          95723UL,
//                          FQFnName.Builtin { name = "+"; version = 0 }
//                        )),
//                        [],
//                        (NEList.doubleton
//                          (EApply(
//                            44444485UL,
//                            (EFnName(
//                              9473UL,
//                              FQFnName.Builtin { name = "+"; version = 0 }
//                            )),
//                            [],
//                            (NEList.doubleton
//                              (EInt64(234213618UL, 5))
//                              (EInt64(923423468UL, 6)))
//                          ))
//                          (EInt64(648327618UL, 7)))
//                      ))
//                      (EInt64(325843618UL, 8)))
//                  )
//                )
//              )
//            symtable = Map.empty
//            typeSymbolTable = Map.empty
//            parameters = NEList.singleton (RT.LPVariable(id 5678, "a")) }
//      ),
//      TFn(NEList.singleton TInt64, TInt64))
//     ("db", DDB "Visitors", TDB TInt64)
//     ("date",
//      DDateTime(
//        DarkDateTime.fromInstant (NodaTime.Instant.ofIsoString "2018-09-14T00:31:41Z")
//      ),
//      TDateTime)
//     ("uuid", DUuid(System.Guid.Parse "7d9e5495-b068-4364-a2cc-3633ab4d13e6"), TUuid)
//     ("uuid0", DUuid(System.Guid.Parse "00000000-0000-0000-0000-000000000000"), TUuid)
//     ("option",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int64 ],
//        "None",
//        []
//      ),
//      TypeReference.option TInt64)
//     ("option2",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int64 ],
//        "Some",
//        [ Dval.int64 15 ]
//      ),
//      TypeReference.option TInt64)
//     ("option3",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.string ],
//        "Some",
//        [ DString "a string" ]
//      ),
//      TypeReference.option TString)
//     ("option4",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int8 ],
//        "Some",
//        [ Dval.int8 15y ]
//      ),
//      TypeReference.option TInt8)
//     ("option5",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.uint8 ],
//        "Some",
//        [ Dval.uint8 15uy ]
//      ),
//      TypeReference.option TUInt8)
//     ("option6",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int16 ],
//        "Some",
//        [ Dval.int16 16s ]
//      ),
//      TypeReference.option TInt16)
//     ("option7",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.uint16 ],
//        "Some",
//        [ Dval.uint16 16us ]
//      ),
//      TypeReference.option TUInt16)
//     ("option8",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int32 ],
//        "Some",
//        [ Dval.int32 32l ]
//      ),
//      TypeReference.option TInt32)
//     ("option9",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.uint32 ],
//        "Some",
//        [ Dval.uint32 32ul ]
//      ),
//      TypeReference.option TUInt32)
//     ("option10",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.int128 ],
//        "Some",
//        [ Dval.int128 128Q ]
//      ),
//      TypeReference.option TInt128)
//     ("option11",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.uint128 ],
//        "Some",
//        [ Dval.uint128 128Z ]
//      ),
//      TypeReference.option TUInt128)
//     ("option12",
//      DEnum(
//        Dval.optionType,
//        Dval.optionType,
//        Dval.ignoreAndUseEmpty [ VT.uint64 ],
//        "Some",
//        [ Dval.uint64 64UL ]
//      ),
//      TypeReference.option TUInt64)
//     ("character", DChar "s", TChar)
//     ("bytes",
//      ((System.Convert.FromBase64String "JyIoXCg=") |> Dval.byteArrayToDvalList),
//      (TList TUInt8))
//     // use image bytes here to test for any weird bytes forms
//     ("bytes2",
//      // TODO: deeply nested data
//      (LibCloud.File.readfileBytes LibCloud.Config.Testdata "sample_image_bytes.png")
//      |> Dval.byteArrayToDvalList,
//      (TList TUInt8))
//     ("simple2Tuple",
//      DTuple(Dval.int64 1, Dval.int64 2, []),
//      TTuple(TInt64, TInt64, []))
//     ("simple3Tuple",
//      DTuple(Dval.int64 1, Dval.int64 2, [ Dval.int64 3 ]),
//      TTuple(TInt64, TInt64, [ TInt64 ]))
//     ("tupleWithUnit",
//      DTuple(Dval.int64 1, Dval.int64 2, [ DUnit ]),
//      TTuple(TInt64, TInt64, [ TUnit ]))
//     ("tupleWithError",
//      DTuple(
//        Dval.int64 1,
//        DEnum(
//          Dval.resultType,
//          Dval.resultType,
//          Dval.ignoreAndUseEmpty [ VT.unknown; VT.string ],
//          "Error",
//          [ DString "error" ]
//        ),
//        []
//      ),
//      TTuple(TInt64, TypeReference.result TInt64 TString, [])) ]

// let sampleDvals : List<string * (Dval * TypeReference)> =
//   List.concat
//     [ List.map (fun (k, v) -> k, DInt64 v, TInt64) interestingInts
//       List.map (fun (k, v) -> k, DFloat v, TFloat) interestingFloats
//       List.map (fun (k, v) -> k, DString v, TString) interestingStrings
//       List.map (fun (k, v) -> k, DString v, TString) naughtyStrings
//       interestingDvals ]
//   |> List.map (fun (k, v, t) -> k, (v, t))

// // Utilties shared among tests
// module Http =
//   type T = { status : string; headers : (string * string) list; body : byte array }

//   let setHeadersToCRLF (text : byte array) : byte array =
//     // We keep our test files with an LF line ending, but the HTTP spec
//     // requires headers (but not the body, nor the first line) to have CRLF
//     // line endings
//     let mutable justSawNewline = false
//     let mutable inBody = false

//     text
//     |> Array.toList
//     |> List.collect (fun b ->
//       if not inBody && b = byte '\n' then
//         if justSawNewline then inBody <- true
//         justSawNewline <- true
//         [ byte '\r'; b ]
//       else
//         justSawNewline <- false
//         [ b ])
//     |> List.toArray

//   let split (response : byte array) : T =
//     // read a single line of bytes (a line ends with \r\n)
//     let rec consume (existing : byte list) (l : byte list) : byte list * byte list =
//       match l with
//       | [] -> [], []
//       | 13uy :: 10uy :: tail -> existing, tail
//       | head :: tail -> consume (existing @ [ head ]) tail

//     // read all headers (ends when we get two \r\n in a row), return headers
//     // and remaining byte string (the body). Assumes the status line is not
//     // present. Headers are returned reversed
//     let rec consumeHeaders
//       (headers : string list)
//       (l : byte list)
//       : string list * byte list =
//       let (line, remaining) = consume [] l

//       if line = [] then
//         (headers, remaining)
//       else
//         let str = line |> Array.ofList |> UTF8.ofBytesUnsafe
//         consumeHeaders (str :: headers) remaining

//     let bytes = Array.toList response

//     // read the status like (eg HTTP 200 OK)
//     let status, bytes = consume [] bytes

//     let headers, body = consumeHeaders [] bytes

//     let headers =
//       headers
//       |> List.reverse
//       |> List.map (fun s ->
//         match String.split ":" s with
//         | k :: vs -> (k, vs |> String.concat ":" |> String.trimLeft)
//         | _ -> Exception.raiseInternal $"not a valid header" [ "header", s ])


//     { status = status |> List.toArray |> UTF8.ofBytesUnsafe
//       headers = headers
//       body = List.toArray body }

// // For an ASP.NET http server, remove the default loggers and add a file logger that
// // saves the output in rundir/logs
// open Microsoft.Extensions.Logging
// open Microsoft.Extensions.DependencyInjection
// open NReco.Logging.File

// let configureLogging
//   (name : string)
//   (builder : Microsoft.Extensions.Logging.ILoggingBuilder)
//   : unit =
//   // This removes the default ConsoleLogger. Having two console loggers (this one and
//   // also the one in Main), caused a deadlock (possibly from having two different
//   // console logging threads).
//   builder
//     .ClearProviders()
//     .Services.AddLogging(fun loggingBuilder ->
//       loggingBuilder.AddFile($"{LibCloud.Config.logDir}{name}.log", append = false)
//       |> ignore<ILoggingBuilder>)
//   |> ignore<IServiceCollection>


// let unwrapExecutionResult
//   (exeResult : RT.ExecutionResult)
//   (state : RT.ExecutionState)
//   : Ply.Ply<RT.Dval> =
//   uply {
//     match exeResult with
//     | Ok dval -> return dval
//     | Error(_callStack, rte) ->
//       let errorMessageFn =
//         RT.FQFnName.fqPackage
//           PackageIDs.Fn.LanguageTools.RuntimeErrors.Error.toErrorMessage

//       let rte = RT.RuntimeError.toDT rte

//       let! rteMessage =
//         LibExecution.Execution.executeFunction
//           state
//           errorMessageFn
//           []
//           (NEList.ofList rte [])

//       match rteMessage with
//       | Ok(RT.DString msg) -> return RT.DString msg
//       | _ -> return RT.DString(string rte)
//   }

// let parsePTExpr (code : string) : Task<PT.Expr> =
//   uply {
//     let! (state : RT.ExecutionState) =
//       let canvasID = System.Guid.NewGuid()
//       executionStateFor pmPT canvasID false false Map.empty

//     let name =
//       RT.FQFnName.FQFnName.Package PackageIDs.Fn.LanguageTools.Parser.parsePTExpr

//     let args = NEList.singleton (RT.DString code)
//     let! execResult = LibExecution.Execution.executeFunction state name [] args

//     match execResult with
//     | Ok dval ->
//       match C2DT.Result.fromDT PT2DT.Expr.fromDT dval identity with
//       | Ok expr -> return expr
//       | Error _ ->
//         return Exception.raiseInternal "Error converting Dval to PT.Expr" []
//     | _ -> return Exception.raiseInternal "Error executing parsePTExpr function" []
//   }
//   |> Ply.toTask

// module Internal =
//   module Test =
//     type PTTest =
//       { name : string; lineNumber : int; actual : PT.Expr; expected : PT.Expr }

//     type RTTest =
//       { name : string; lineNumber : int; actual : RT.Expr; expected : RT.Expr }

//     let typeName = FQTypeName.fqPackage PackageIDs.Type.Internal.Test.ptTest

//     let toDt (t : PTTest) : Dval =
//       let fields =
//         [ "name", DString t.name
//           "lineNumber", DInt64 t.lineNumber
//           "actual", PT2DT.Expr.toDT t.actual
//           "expected", PT2DT.Expr.toDT t.expected ]
//       DRecord(typeName, typeName, [], Map fields)

//     let fromDT (d : Dval) : PTTest =
//       match d with
//       | DRecord(_, _, _, fields) ->
//         { name = fields |> D.stringField "name"
//           lineNumber = fields |> D.intField "lineNumber"
//           actual = fields |> D.field "actual" |> PT2DT.Expr.fromDT
//           expected = fields |> D.field "expected" |> PT2DT.Expr.fromDT }
//       | _ -> Exception.raiseInternal "Invalid Test" []
