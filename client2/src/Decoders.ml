module OrigJson = Json
open Tea
open! Porting
module RT = Runtime
open Types
module JSD = Json_decode_extended
open Json_decode_extended

let id j =
  ID (int j)

let tlid j =
  TLID (int j)

let pos j : pos =
  { x = field "x" int j
  ; y = field "y" int j
  }

let vPos j : vPos =
  { vx = field "vx" int j
  ; vy = field "vy" int j
  }


let blankOr d =
  variants
    [ ("Filled", variant2 (fun id v -> F (id, v)) id d)
    ; ("Blank", variant1 (fun id -> Blank id) id)
    ]

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
    ; ("PParamTipe", dv1 (fun x -> PParamTipe x) (blankOr tipe)) ]
    j

(* let tlidOf (op : op) : tlid = *)
(*   match op with *)
(*   | SetHandler (tlid, _, _) -> tlid *)
(*   | CreateDB (tlid, _, _) -> tlid *)
(*   | AddDBCol (tlid, _, _) -> tlid *)
(*   | SetDBColName (tlid, _, _) -> tlid *)
(*   | ChangeDBColName (tlid, _, _) -> tlid *)
(*   | SetDBColType (tlid, _, _) -> tlid *)
(*   | ChangeDBColType (tlid, _, _) -> tlid *)
(*   | DeprecatedInitDbm (tlid, _, _, _, _) -> tlid *)
(*   | TLSavepoint tlid -> tlid *)
(*   | UndoTL tlid -> tlid *)
(*   | RedoTL tlid -> tlid *)
(*   | DeleteTL tlid -> tlid *)
(*   | MoveTL (tlid, _) -> tlid *)
(*   | SetFunction f -> f.ufTLID *)
(*   | DeleteFunction tlid -> tlid *)
(*   | SetExpr (tlid, _, _) -> tlid *)
(*   | CreateDBMigration (tlid, _, _, _) -> tlid *)
(*   | AddDBColToDBMigration (tlid, _, _) -> tlid *)
(*   | SetDBColNameInDBMigration (tlid, _, _) -> tlid *)
(*   | SetDBColTypeInDBMigration (tlid, _, _) -> tlid *)
(*   | AbandonDBMigration tlid -> tlid *)
(*   | DeleteColInDBMigration (tlid, _) -> tlid *)
(*  *)
(* let serializableEditor : serializableEditor decoder = *)
(*   JSDP.decode SerializableEditor *)
(*   |> JSDP.optional "clipboard" (maybe pointerData) None *)
(*   |> JSDP.optional "timersEnabled" bool true *)
(*   |> JSDP.optional "cursorState" cursorState Deselected *)
(*   |> JSDP.optional "lockedHandlers" (list tlid) [] *)
(*  *)

and serializableEditor (j: Js.Json.t) : serializableEditor =
  { clipboard = orNull (field "clipboard" (optional pointerData)) None j
  ; timersEnabled = orNull (field "timersEnabled" bool) true j
  ; cursorState = orNull (field "cursorState" cursorState) Deselected j
  ; lockedHandlers = orNull (field "lockedHandlers" (list tlid)) [] j
  }

and cursorState j =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dv4 = variant4 in
  variants
    [ ("Selecting", dv2 (fun a b -> Selecting (a,b)) tlid (optional (id)))
    ; ("Entering", dv1 (fun a -> Entering a) entering)
    ; ("Dragging", dv4 (fun a b c d -> Dragging (a,b,c,d)) tlid vPos bool cursorState)
    ; ("Deselected", dv0 Deselected)
    ; ("SelectingCommand", dv2 (fun a b -> SelectingCommand (a,b)) tlid id)
    ]
    j

and entering j =
  let dv1 = variant1 in
  let dv2 = variant2 in
  variants
    [ ("Creating", dv1 (fun x -> Creating x) pos)
    ; ("Filling", dv2 (fun a b -> Filling (a,b)) tlid id)
    ]
    j

and expr j : expr =
  blankOr nExpr j

and nExpr j : nExpr =
  let de = expr in
  let did = id in
  let dv4 = variant4 in
  let dv3 = variant3 in
  let dv2 = variant2 in
  let dv1 = variant1 in
  variants
    [ ("Let", dv3 (fun a b c-> Let (a,b,c)) (blankOr string) de de)
    ; ("Value", dv1 (fun x -> Value x) string)
    ; ("If", dv3 (fun a b c -> If (a,b,c)) de de de)
    ; ( "FnCall"
      , dv2 (fun a b -> FnCall (a, b, NoRail)) string (list de) )
    ; ( "FnCallSendToRail"
      , dv2 (fun a b -> FnCall (a, b, Rail)) string (list de) )
    ; ("Lambda", dv2 (fun a b -> Lambda (a,b)) (list (blankOr string)) de)
    ; ("Variable", dv1 (fun x -> Variable x) string)
    ; ("Thread", dv1 (fun x -> Thread x) (list de))
    ; ("FieldAccess", dv2 (fun a b -> FieldAccess (a,b)) de (blankOr string))
    ; ("ListLiteral", dv1 (fun x -> ListLiteral x) (list de))
    ; ( "ObjectLiteral"
      , dv1 (fun x -> ObjectLiteral x) (list (tuple2 (blankOr string) de))
      )
    ; ("FeatureFlag", dv4 (fun a b c d -> FeatureFlag (a,b,c,d)) (blankOr string) de de de)
    ]
    j

(* let analysisResults : analysisResults decoder = *)
(*   let toAResult liveValues availableVarnames = *)
(*     { liveValues= DE.mapKeys (Util.toIntWithDefault 0) liveValues *)
(*     ; availableVarnames= DE.mapKeys (Util.toIntWithDefault 0) availableVarnames *)
(*     } *)
(*   in *)
(*   JSDP.decode toAResult *)
(*   |> JSDP.required "live_values" (dict decodeDval) *)
(*   |> JSDP.required "available_varnames" (dict (list string)) *)
(*  *)
(* let analysisEnvelope : (traceID * analysisResults) decoder = *)
(*   JSONUtils.decodetuple2 string decodeAnalysisResults *)
(*  *)
(* let handlerSpec : handlerSpec decoder = *)
(*   let toHS module_ name modifier = {module_; name; modifier} in *)
(*   JSDP.decode toHS *)
(*   |> JSDP.required "module" (blankOr string) *)
(*   |> JSDP.required "name" (blankOr string) *)
(*   |> JSDP.required "modifier" (blankOr string) *)
(*  *)
(* let handler : handler decoder = *)
(*   let toHandler ast spec tlid = {ast; spec; tlid} in *)
(*   JSDP.decode toHandler *)
(*   |> JSDP.required "ast" decodeExpr *)
(*   |> JSDP.required "spec" decodeHandlerSpec *)
(*   |> JSDP.required "tlid" tlid *)
(*  *)
(* let tipeString : string decoder = map RT.tipe2str decodeTipe *)
(*  *)
(* let dBColList : dBColumn list decoder = *)
(*   list *)
(*     (decodetuple2 (blankOr string) (blankOr decodeTipeString)) *)
(*  *)
(* let dBMigrationState : dBMigrationState decoder = *)
(*   let dv0 = variant0 in *)
(*   variants *)
(*     [ ("DBMigrationAbandoned", dv0 DBMigrationAbandoned) *)
(*     ; ("DBMigrationInitialized", dv0 DBMigrationInitialized) ] *)
(*  *)
(* let dBMigration : dBMigration decoder = *)
(*   let toDBM sv v s cols rollf rollb = *)
(*     { startingVersion= sv *)
(*     ; version= v *)
(*     ; state= s *)
(*     ; cols *)
(*     ; rollforward= rollf *)
(*     ; rollback= rollb } *)
(*   in *)
(*   JSDP.decode toDBM *)
(*   |> JSDP.required "starting_version" int *)
(*   |> JSDP.required "version" int *)
(*   |> JSDP.required "state" decodeDBMigrationState *)
(*   |> JSDP.required "cols" decodeDBColList *)
(*   |> JSDP.required "rollforward" decodeExpr *)
(*   |> JSDP.required "rollback" decodeExpr *)
(*  *)
(* let dB : dB decoder = *)
(*   let toDB tlid name cols version old active = *)
(*     { dbTLID= TLID tlid *)
(*     ; dbName= name *)
(*     ; cols *)
(*     ; version *)
(*     ; oldMigrations= old *)
(*     ; activeMigration= active } *)
(*   in *)
(*   JSDP.decode toDB *)
(*   |> JSDP.required "tlid" int *)
(*   |> JSDP.required "name" string *)
(*   |> JSDP.required "cols" decodeDBColList *)
(*   |> JSDP.required "version" int *)
(*   |> JSDP.required "old_migrations" (list decodeDBMigration) *)
(*   |> JSDP.required "active_migration" (maybe decodeDBMigration) *)
(*  *)
(* let toplevel : toplevel decoder = *)
(*   let toToplevel id x y data = {id; pos= {x; y}; data} in *)
(*   let variant = *)
(*     variants *)
(*       [ ("Handler", variant1 TLHandler decodeHandler) *)
(*       ; ("DB", variant1 TLDB decodeDB) ] *)
(*   in *)
(*   JSDP.decode toToplevel *)
(*   |> JSDP.required "tlid" tlid *)
(*   |> JSDP.requiredAt ["pos"; "x"] int *)
(*   |> JSDP.requiredAt ["pos"; "y"] int *)
(*   |> JSDP.required "data" variant *)
(*  *)
and tipe j : tipe =
  let dv0 = variant0 in
  let dv1 = variant1 in
  variants
    [ ("TInt", dv0 TInt)
    ; ("TStr", dv0 TStr)
    ; ("TChar", dv0 TChar)
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
    ; ("TID", dv0 TID)
    ; ("TDate", dv0 TDate)
    ; ("TTitle", dv0 TTitle)
    ; ("TUrl", dv0 TUrl)
    ; ("TPassword", dv0 TPassword)
    ; ("TUuid", dv0 TUuid)
    ; ("TOption", dv0 TOption)
    ; ("TErrorRail", dv0 TErrorRail)
    ; ("TBelongsTo", dv1 (fun x -> TBelongsTo x) string)
    ; ("THasMany", dv1 (fun x -> THasMany x) string)
    ; ("TDbList", dv1 (fun x -> TDbList x) tipe)
    ]
    j


(* let userFunctionParameter : userFunctionParameter decoder = *)
(*   let toParam name tipe args option desc = *)
(*     { ufpName= name *)
(*     ; ufpTipe= tipe *)
(*     ; ufpBlock_args= args *)
(*     ; ufpOptional= option *)
(*     ; ufpDescription= desc } *)
(*   in *)
(*   JSDP.decode toParam *)
(*   |> JSDP.required "name" (blankOr string) *)
(*   |> JSDP.required "tipe" (blankOr decodeTipe) *)
(*   |> JSDP.required "block_args" (list string) *)
(*   |> JSDP.required "optional" bool *)
(*   |> JSDP.required "description" string *)
(*  *)
(* let userFunctionMetadata : userFunctionMetadata decoder = *)
(*   let toFn name params desc returnTipe infix = *)
(*     { ufmName= name *)
(*     ; ufmParameters= params *)
(*     ; ufmDescription= desc *)
(*     ; ufmReturnTipe= returnTipe *)
(*     ; ufmInfix= infix } *)
(*   in *)
(*   JSDP.decode toFn *)
(*   |> JSDP.required "name" (blankOr string) *)
(*   |> JSDP.required "parameters" (list decodeUserFunctionParameter) *)
(*   |> JSDP.required "description" string *)
(*   |> JSDP.required "return_type" (blankOr decodeTipe) *)
(*   |> JSDP.required "infix" bool *)
(*  *)
(* let userFunction : userFunction decoder = *)
(*   let toUserFn id meta ast = {ufTLID= id; ufMetadata= meta; ufAST= ast} in *)
(*   JSDP.decode toUserFn *)
(*   |> JSDP.required "tlid" tlid *)
(*   |> JSDP.required "metadata" decodeUserFunctionMetadata *)
(*   |> JSDP.required "ast" decodeExpr *)
(*  *)
(* let 404 : fourOhFour decoder = *)
(*   map3 FourOhFour (index 0 string) (index 1 string) *)
(*     (index 2 string) *)
(*  *)
(* let inputValueDict : inputValueDict decoder = *)
(*   map Dict.fromList (list (decodetuple2 string decodeDval)) *)
(*  *)
(* let functionResult : functionResult decoder = *)
(*   let toFunctionResult (fnName, id, hash, value) = *)
(*     {fnName; callerID= id; argHash= hash; value} *)
(*   in *)
(*   map toFunctionResult *)
(*     (JSONUtils.decodeQuadriple string id string decodeDval) *)
(*  *)
(* let traces : traces decoder = *)
(*   map Dict.fromList (list (decodetuple2 int (list decodeTrace))) *)
(*  *)
(* let trace : trace decoder = *)
(*   let toTrace id input functionResults = *)
(*     {traceID= id; input; functionResults} *)
(*   in *)
(*   JSDP.decode toTrace *)
(*   |> JSDP.required "id" string *)
(*   |> JSDP.required "input" decodeInputValueDict *)
(*   |> JSDP.required "function_results" (list decodeFunctionResult) *)
(*  *)
(* let executeFunctionTarget : (tlid * id) decoder = *)
(*   map2 Tuple2.create (index 0 tlid) (index 1 id) *)
(*  *)
(* let rPC : rpcResult decoder = *)
(*   JSDP.decode Tuple6.create *)
(*   |> JSDP.required "toplevels" (list decodeToplevel) *)
(*   |> JSDP.required "deleted_toplevels" (list decodeToplevel) *)
(*   |> JSDP.required "new_traces" decodeTraces *)
(*   |> JSDP.required "global_varnames" (list string) *)
(*   |> JSDP.required "user_functions" (list decodeUserFunction) *)
(*   |> JSDP.required "unlocked_dbs" (list tlid) *)
(*  *)
(* let getAnalysisRPC : getAnalysisResult decoder = *)
(*   JSDP.decode Tuple4.create *)
(*   |> JSDP.required "traces" decodeTraces *)
(*   |> JSDP.required "global_varnames" (list string) *)
(*   |> JSDP.required "404s" (list decode404) *)
(*   |> JSDP.required "unlocked_dbs" (list tlid) *)
(*  *)
(* let initialLoadRPC : initialLoadResult decoder = decodeRPC *)
(*  *)
(* let executeFunctionRPC : executeFunctionRPCResult decoder = *)
(*   JSDP.decode Tuple2.create *)
(*   |> JSDP.required "result" decodeDval *)
(*   |> JSDP.required "hash" string *)
(*  *)
(* let isLiteralString (s : string) : bool = *)
(*   match parseDvalLiteral s with None -> false | Some dv -> RT.isLiteral dv *)
(*  *)
(* let typeOfLiteralString (s : string) : tipe = *)
(*   match parseDvalLiteral s with None -> TIncomplete | Some dv -> RT.typeOf dv *)
(*  *)
and parseDvalLiteral (str : string) : dval option =
  let firstChar = String.uncons str |> Option.map Tuple.first in
  if String.toLower str = "nothing" then Some (DOption OptNothing)
  else
    match String.toList str with
    | ['\''; c; '\''] -> Some (DChar c)
    | '"' :: rest ->
        if List.last rest = Some '"' then
          List.init rest |> Option.withDefault [] |> String.fromList
          |> (fun x -> DStr x)
          |> fun x -> Some x
        else None
    | _ -> str |> OrigJson.parseOrRaise |> parseBasicDval |> (fun x -> Some x)

and parseBasicDval str : dval =
  oneOf
    [ map (fun x -> DInt x) int
    ; map (fun x -> DFloat x) float
    ; map (fun x -> DBool x) bool
    ; nullAs DNull
    ; map (fun x -> DStr x) string
    ; map (fun x -> DList x) (list dval)
    ]
    str

and dval j : dval =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dd = dval in
  let optionT =
    variants [ ("OptJust", dv1 (fun x -> OptJust x) dd)
             ; ("OptNothing", dv0 OptNothing)]
  in
  let dhttp =
    variants
      [ ("Redirect", dv1 (fun x -> Redirect x) string)
      ; ( "Response"
        , dv2 (fun a b -> Response (a,b)) int
            (list (tuple2 string string)) ) ]
  in
  variants
    [ ("DInt", dv1 (fun x -> DInt x) int)
    ; ("DFloat", dv1 (fun x -> DFloat x) float)
    ; ("DBool", dv1 (fun x -> DBool x) bool)
    ; ("DNull", dv0 DNull)
    ; ("DStr", dv1 (fun x -> DStr x) string)
    ; ("DList", dv1 (fun x -> DList x) (list dd))
    ; ("DObj", dv1 (fun x -> DObj x) (dict dd))
    ; ("DIncomplete", dv0 DIncomplete)
    ; ("DError", dv1 (fun x -> DError x) string)
    ; ("DBlock", dv0 DBlock)
    ; ("DErrorRail", dv1 (fun x -> DErrorRail x) dd)
    ; ( "DResp"
      , dv1
          (fun (h, dv) -> DResp (h, dv))
          (tuple2 dhttp dd) )
    ; ("DDB", dv1 (fun x -> DDB x) string)
    ; ("DID", dv1 (fun x -> DID x) string)
    ; ("DDate", dv1 (fun x -> DDate x) string)
    ; ("DTitle", dv1 (fun x -> DTitle x) string)
    ; ("DUrl", dv1 (fun x -> DUrl x) string)
    ; ( "DPassword"
      , dv1
          ( Base64.decode
          >> Result.withDefault "<Internal error in base64 decoding>"
          >> (fun x -> DPassword x))
          string )
    ; ("DUuid", dv1 (fun x -> DUuid x) string)
    ; ("DOption", dv1 (fun x -> DOption x) optionT)
    ]
    j


