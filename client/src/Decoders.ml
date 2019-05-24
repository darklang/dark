open Tc
open Types
open Json_decode_extended

(* Dark *)
module RT = Runtime

external stringify : Js.Json.t -> string = "JSON.stringify" [@@bs.val]

(* identifiers are strings to the bucklescript client -- it knows nothing
 * about them being parseable as ints. if it doesn't look like a string
 * to bs-json we'll just json stringify it and use that *)
let wireIdentifier j = try string j with _ -> int j |> string_of_int

let id j = ID (wireIdentifier j)

let tlid j = TLID (wireIdentifier j)

let pos j : pos = {x = field "x" int j; y = field "y" int j}

let vPos j : vPos = {vx = field "vx" int j; vy = field "vy" int j}

let blankOr d =
  variants
    [ ("Filled", variant2 (fun id v -> F (id, v)) id d)
    ; ("Blank", variant1 (fun id -> Blank id) id)
    ; ("Partial", variant2 (fun id v -> Partial (id, v)) id string) ]


let rec pointerData j : pointerData =
  let dv1 = variant1 in
  variants
    [ ("PVarBind", dv1 (fun x -> PVarBind x) (blankOr string))
    ; ("PEventName", dv1 (fun x -> PEventName x) (blankOr string))
    ; ("PEventModifier", dv1 (fun x -> PEventModifier x) (blankOr string))
    ; ("PEventSpace", dv1 (fun x -> PEventSpace x) (blankOr string))
    ; ("PExpr", dv1 (fun x -> PExpr x) expr)
    ; ("PField", dv1 (fun x -> PField x) (blankOr string))
    ; ("PDBColName", dv1 (fun x -> PDBColName x) (blankOr string))
    ; ("PDBColType", dv1 (fun x -> PDBColType x) (blankOr string))
    ; ("PFFMsg", dv1 (fun x -> PFFMsg x) (blankOr string))
    ; ("PFnName", dv1 (fun x -> PFnName x) (blankOr string))
    ; ("PParamName", dv1 (fun x -> PParamName x) (blankOr string))
    ; ("PParamTipe", dv1 (fun x -> PParamTipe x) (blankOr tipe))
    ; ("PPattern", dv1 (fun x -> PPattern x) pattern) ]
    j


and handlerState j : handlerState =
  j
  |> variants
       [ ("HandlerExpanded", variant0 HandlerExpanded)
       ; ("HandlerPrepCollapse", variant0 HandlerPrepCollapse)
       ; ("HandlerCollapsing", variant0 HandlerCollapsing)
       ; ("HandlerCollapsed", variant0 HandlerCollapsed)
       ; ("HandlerExpanding", variant0 HandlerExpanding) ]


and handlerProp j : handlerProp =
  { handlerLock = field "handlerLock" bool j
  ; handlerState = field "handlerState" handlerState j
  ; hoveringVariableName = field "hoveringVariableName" (optional string) j }


and serializableEditor (j : Js.Json.t) : serializableEditor =
  (* always make these optional so that we don't crash the page when we *)
  (* change the structure *)
  { timersEnabled = orNull (field "timersEnabled" bool) true j
  ; cursorState =
      ( try orNull (field "cursorState" cursorState) Deselected j with _ ->
          Deselected )
  ; routingTableOpenDetails =
      ( try orNull (field "routingTableOpenDetails" tcStrSet) StrSet.empty j
        with _ -> StrSet.empty )
  ; tlCursors =
      ( try orNull (field "tlCursors" (dict traceID)) StrDict.empty j
        with _ -> StrDict.empty )
  ; featureFlags =
      ( try orNull (field "featureFlags" (dict bool)) StrDict.empty j
        with _ -> StrDict.empty )
  ; handlerProps =
      ( try orNull (field "handlerProps" (dict handlerProp)) StrDict.empty j
        with _ -> StrDict.empty )
  ; canvasPos = orNull (field "canvasPos" pos) Defaults.origin j }


and cursorState j =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dv4 = variant4 in
  variants
    [ ("Selecting", dv2 (fun a b -> Selecting (a, b)) tlid (optional id))
    ; ("Entering", dv1 (fun a -> Entering a) entering)
    ; ( "Dragging"
      , dv4 (fun a b c d -> Dragging (a, b, c, d)) tlid vPos bool cursorState
      )
    ; ("Deselected", dv0 Deselected)
    ; ("SelectingCommand", dv2 (fun a b -> SelectingCommand (a, b)) tlid id)
    ; ("FluidEntering", dv1 (fun a -> FluidEntering a) tlid) ]
    j


and entering j =
  let dv1 = variant1 in
  let dv2 = variant2 in
  variants
    [ ("Creating", dv1 (fun x -> Creating x) pos)
    ; ("Filling", dv2 (fun a b -> Filling (a, b)) tlid id) ]
    j


and expr j : expr =
  match blankOr nExpr j with
  | F (ID id, FnCall (F (ID "fncall", name), exprs, rail)) ->
      F (ID id, FnCall (F (ID (id ^ "_name"), name), exprs, rail))
  | other ->
      other


and nExpr j : nExpr =
  let de = expr in
  let dv4 = variant4 in
  let dv3 = variant3 in
  let dv2 = variant2 in
  let dv1 = variant1 in
  (* In order to ignore the server for now, we tweak from one format *)
  (* to the other. *)
  variants
    [ ("Let", dv3 (fun a b c -> Let (a, b, c)) (blankOr string) de de)
    ; ("Value", dv1 (fun x -> Value x) string)
    ; ("If", dv3 (fun a b c -> If (a, b, c)) de de de)
    ; ( "FnCall"
      , dv2
          (fun a b -> FnCall (F (ID "fncall", a), b, NoRail))
          string
          (list de) )
    ; ( "FnCallSendToRail"
      , dv2 (fun a b -> FnCall (F (ID "fncall", a), b, Rail)) string (list de)
      )
    ; ("Lambda", dv2 (fun a b -> Lambda (a, b)) (list (blankOr string)) de)
    ; ("Variable", dv1 (fun x -> Variable x) string)
    ; ("Thread", dv1 (fun x -> Thread x) (list de))
    ; ("FieldAccess", dv2 (fun a b -> FieldAccess (a, b)) de (blankOr string))
    ; ("ListLiteral", dv1 (fun x -> ListLiteral x) (list de))
    ; ( "ObjectLiteral"
      , dv1 (fun x -> ObjectLiteral x) (list (tuple2 (blankOr string) de)) )
    ; ( "FeatureFlag"
      , dv4 (fun a b c d -> FeatureFlag (a, b, c, d)) (blankOr string) de de de
      )
    ; ("Match", dv2 (fun a b -> Match (a, b)) de (list (tuple2 pattern de)))
    ; ( "Constructor"
      , dv2 (fun a b -> Constructor (a, b)) (blankOr string) (list de) ) ]
    j


and pattern j : pattern = blankOr nPattern j

and nPattern j : nPattern =
  variants
    [ ("PVariable", variant1 (fun a -> PVariable a) string)
    ; ("PLiteral", variant1 (fun a -> PLiteral a) string)
    ; ( "PConstructor"
      , variant2 (fun a b -> PConstructor (a, b)) string (list pattern) ) ]
    j


and lvDict j : lvDict = j |> dict dval

and analysisResults j : analysisResults =
  {liveValues = field "live_values" lvDict j}


and analysisEnvelope j : traceID * analysisResults =
  (tuple2 string analysisResults) j


and handlerSpec j : handlerSpec =
  { module_ = field "module" (blankOr string) j
  ; name = field "name" (blankOr string) j
  ; modifier = field "modifier" (blankOr string) j }


and handler j : handler =
  { ast = field "ast" expr j
  ; spec = field "spec" handlerSpec j
  ; tlid = field "tlid" tlid j }


and tipeString j : string = map RT.tipe2str tipe j

and dbColList j : dBColumn list =
  list (tuple2 (blankOr string) (blankOr tipeString)) j


and dbMigrationState j : dBMigrationState =
  let dv0 = variant0 in
  variants
    [ ("DBMigrationAbandoned", dv0 DBMigrationAbandoned)
    ; ("DBMigrationInitialized", dv0 DBMigrationInitialized) ]
    j


and dbMigration j : dBMigration =
  { startingVersion = field "starting_version" int j
  ; version = field "version" int j
  ; state = field "state" dbMigrationState j
  ; cols = field "cols" dbColList j
  ; rollforward = field "rollforward" expr j
  ; rollback = field "rollback" expr j }


and db j : dB =
  { dbTLID = field "tlid" tlid j
  ; dbName = field "name" (blankOr string) j
  ; cols = field "cols" dbColList j
  ; version = field "version" int j
  ; oldMigrations = field "old_migrations" (list dbMigration) j
  ; activeMigration = field "active_migration" (optional dbMigration) j }


and toplevel j : toplevel =
  let variant =
    variants
      [ ("Handler", variant1 (fun x -> TLHandler x) handler)
      ; ("DB", variant1 (fun x -> TLDB x) db) ]
  in
  { id = field "tlid" tlid j
  ; pos = field "pos" pos j
  ; data = field "data" variant j }


and tipe j : tipe =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  variants
    [ ("TInt", dv0 TInt)
    ; ("TStr", dv0 TStr)
    ; ("TBool", dv0 TBool)
    ; ("TFloat", dv0 TFloat)
    ; ("TObj", dv0 TObj)
    ; ("TList", dv0 TList)
    ; ("TAny", dv0 TAny)
    ; ("TNull", dv0 TNull)
    ; ("TBlock", dv0 TBlock)
    ; ("TIncomplete", dv0 TIncomplete)
    ; ("TError", dv0 TError)
    ; ("TResp", dv0 TResp)
    ; ("TDB", dv0 TDB)
    ; ("TDate", dv0 TDate)
    ; ("TPassword", dv0 TPassword)
    ; ("TUuid", dv0 TUuid)
    ; ("TOption", dv0 TOption)
    ; ("TErrorRail", dv0 TErrorRail)
    ; ("TBelongsTo", dv1 (fun x -> TBelongsTo x) string)
    ; ("THasMany", dv1 (fun x -> THasMany x) string)
    ; ("TDbList", dv1 (fun x -> TDbList x) tipe)
    ; ("TUserType", dv2 (fun n v -> TUserType (n, v)) string int) ]
    j


and userFunctionParameter j : userFunctionParameter =
  { ufpName = field "name" (blankOr string) j
  ; ufpTipe = field "tipe" (blankOr tipe) j
  ; ufpBlock_args = field "block_args" (list string) j
  ; ufpOptional = field "optional" bool j
  ; ufpDescription = field "description" string j }


and userFunctionMetadata j : userFunctionMetadata =
  { ufmName = field "name" (blankOr string) j
  ; ufmParameters = field "parameters" (list userFunctionParameter) j
  ; ufmDescription = field "description" string j
  ; ufmReturnTipe = field "return_type" (blankOr tipe) j
  ; ufmInfix = field "infix" bool j }


and userFunction j : userFunction =
  { ufTLID = field "tlid" tlid j
  ; ufMetadata = field "metadata" userFunctionMetadata j
  ; ufAST = field "ast" expr j }


and fof j : fourOhFour =
  { space = index 0 string j
  ; path = index 1 string j
  ; modifier = index 2 string j
  ; timestamp = index 3 string j
  ; traceID = index 4 traceID j }


and deployStatus j : deployStatus =
  let sumtypes =
    [("Deployed", variant0 Deployed); ("Deploying", variant0 Deploying)]
  in
  j |> variants sumtypes


and jsDate j : Js.Date.t = Js.Date.fromString (string j)

and sDeploy j : staticDeploy =
  { deployHash = field "deploy_hash" string j
  ; url = field "url" string j
  ; lastUpdate = field "last_update" jsDate j
  ; status = field "status" deployStatus j }


and inputValueDict j : inputValueDict =
  j |> list (tuple2 string dval) |> StrDict.fromList


and functionResult j : functionResult =
  let fnName, callerID, argHash, value = tuple4 string id string dval j in
  {fnName; callerID; argHash; value}


and traceID j : traceID = wireIdentifier j

and traces j : traces =
  j |> list (tuple2 wireIdentifier (list trace)) |> StrDict.fromList


and traceData j : traceData =
  { input = field "input" inputValueDict j
  ; timestamp = field "timestamp" string j
  ; functionResults = field "function_results" (list functionResult) j }


and trace j : trace = pair traceID (optional traceData) j

and userRecordField j =
  { urfName = field "name" (blankOr string) j
  ; urfTipe = field "tipe" (blankOr tipe) j }


and userTipeDefinition j =
  variants
    [("UTRecord", variant1 (fun x -> UTRecord x) (list userRecordField))]
    j


and userTipe j =
  { utTLID = field "tlid" tlid j
  ; utName = field "name" (blankOr string) j
  ; utVersion = field "version" int j
  ; utDefinition = field "definition" userTipeDefinition j }


and addOpRPCResult j : addOpRPCResult =
  { toplevels = field "toplevels" (list toplevel) j
  ; deletedToplevels = field "deleted_toplevels" (list toplevel) j
  ; userFunctions = field "user_functions" (list userFunction) j
  ; deletedUserFunctions = field "deleted_user_functions" (list userFunction) j
  ; userTipes = field "user_tipes" (list userTipe) j
  ; deletedUserTipes = field "deleted_user_tipes" (list userTipe) j }


and getUnlockedDBsRPCResult j : getUnlockedDBsRPCResult =
  j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList


and getTraceDataRPCResult j : getTraceDataRPCResult =
  {trace = field "trace" trace j}


and dbStats j : dbStats =
  {count = field "count" int j; example = field "example" (optional dval) j}


and dbStatsStore j : dbStatsStore = dict dbStats j

and dbStatsRPCResult j = dbStatsStore j

and initialLoadRPCResult j : initialLoadRPCResult =
  { toplevels = field "toplevels" (list toplevel) j
  ; deletedToplevels = field "deleted_toplevels" (list toplevel) j
  ; userFunctions = field "user_functions" (list userFunction) j
  ; deletedUserFunctions = field "deleted_user_functions" (list userFunction) j
  ; unlockedDBs =
      j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList
  ; fofs = field "fofs" (list fof) j
  ; staticDeploys = field "assets" (list sDeploy) j
  ; traces = field "traces" (list (pair tlid traceID)) j
  ; userTipes = field "user_tipes" (list userTipe) j
  ; deletedUserTipes = field "deleted_user_tipes" (list userTipe) j }


and executeFunctionRPCResult j : executeFunctionRPCResult =
  ( field "result" dval j
  , field "hash" string j
  , field "touched_tlids" (list tlid) j )


and triggerCronRPCResult j : triggerCronRPCResult =
  (field "trace_id" string j, field "tlid" (list tlid) j)


and saveTestRPCResult j : saveTestRPCResult = string j

(* -------------------------- *)
(* Dval (some here because of cyclic dependencies) *)
(* ------------------------- *)
and isLiteralRepr (s : string) : bool =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then true
  else
    match parseDvalLiteral s with None -> false | Some dv -> RT.isLiteral dv


and typeOfLiteral (s : string) : tipe =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then TStr
  else
    match parseDvalLiteral s with
    | None ->
        TIncomplete
    | Some dv ->
        RT.typeOf dv


(* Ported directly from Dval.parse in the backend *)
and parseDvalLiteral (str : string) : dval option =
  match String.toList str with
  | ['\''; c; '\''] ->
      Some (DCharacter (String.fromList [c]))
  | '"' :: rest ->
      if List.last rest = Some '"'
      then
        List.init rest
        |> Option.withDefault ~default:[]
        |> String.fromList
        |> fun x -> Some (DStr x)
      else None
  | _ ->
    (try Some (parseBasicDval (Json.parseOrRaise str)) with _ -> None)


and parseBasicDval str : dval =
  oneOf
    [ map (fun x -> DInt x) int
    ; map (fun x -> DFloat x) Json_decode_extended.float
    ; map (fun x -> DBool x) bool
    ; nullAs DNull
    ; map (fun x -> DStr x) string ]
    str


and dval j : dval =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dd = dval in
  let optionT =
    variants
      [("OptJust", dv1 (fun x -> OptJust x) dd); ("OptNothing", dv0 OptNothing)]
  in
  let resultT =
    variants
      [ ("ResOk", dv1 (fun x -> ResOk x) dd)
      ; ("ResError", dv1 (fun x -> ResError x) dd) ]
  in
  let dhttp =
    variants
      [ ("Redirect", dv1 (fun x -> Redirect x) string)
      ; ( "Response"
        , dv2 (fun a b -> Response (a, b)) int (list (tuple2 string string)) )
      ]
  in
  variants
    [ ("DInt", dv1 (fun x -> DInt x) int)
    ; ("DFloat", dv1 (fun x -> DFloat x) Json_decode_extended.float)
    ; ("DBool", dv1 (fun x -> DBool x) bool)
    ; ("DNull", dv0 DNull)
    ; ("DCharacter", dv1 (fun x -> DCharacter x) string)
    ; ("DStr", dv1 (fun x -> DStr x) string)
    ; ("DList", dv1 (fun x -> DList x) (list dd))
    ; ("DObj", dv1 (fun x -> DObj x) (dict dd))
    ; ("DIncomplete", dv0 DIncomplete)
    ; ("DError", dv1 (fun x -> DError x) string)
    ; ("DBlock", dv0 DBlock)
    ; ("DErrorRail", dv1 (fun x -> DErrorRail x) dd)
    ; ("DResp", dv1 (fun (h, dv) -> DResp (h, dv)) (tuple2 dhttp dd))
    ; ("DDB", dv1 (fun x -> DDB x) string)
    ; ("DDate", dv1 (fun x -> DDate x) string)
    ; ("DPassword", dv1 (fun x -> DPassword x) string)
    ; ("DUuid", dv1 (fun x -> DUuid x) string)
    ; ("DOption", dv1 (fun x -> DOption x) optionT)
    ; ("DResult", dv1 (fun x -> DResult x) resultT)
    ; ( "DBytes"
      , dv1
          (fun x ->
            let x = x |> Webapi.Base64.btoa |> Bytes.of_string in
            DBytes x )
          string ) ]
    j


let exception_ j : exception_ =
  { short = field "short" string j
  ; long = field "long" (optional string) j
  ; exceptionTipe = field "tipe" string j
  ; actual = field "actual" (optional string) j
  ; actualType = field "actual_tipe" (optional string) j
  ; expected = field "expected" (optional string) j
  ; result = field "result" (optional string) j
  ; resultType = field "result_tipe" (optional string) j
  ; info = field "info" (dict string) j
  ; workarounds = field "workarounds" (list string) j }


(* Wrap JSON decoders using bs-json's format, into TEA's HTTP expectation format *)
let wrapExpect (fn : Js.Json.t -> 'a) : string -> ('ok, string) Tea.Result.t =
 fun j ->
  try Ok (fn (Json.parseOrRaise j)) with
  | DecodeError e ->
      Error e
  | e ->
      Error (Printexc.to_string e)


(* Wrap JSON decoders using bs-json's format, into TEA's JSON decoder format *)
let wrapDecoder (fn : Js.Json.t -> 'a) : (Js.Json.t, 'a) Tea.Json.Decoder.t =
  Decoder
    (fun value ->
      try Tea_result.Ok (fn value) with
      | DecodeError e ->
          Tea_result.Error e
      | e ->
          Tea_result.Error ("Json error: " ^ Printexc.to_string e) )
