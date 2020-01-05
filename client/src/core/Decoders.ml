open Prelude
open Json.Decode

(* Dark *)
module TL = Toplevel
module RT = Runtime

external stringify : Js.Json.t -> string = "JSON.stringify" [@@bs.val]

(* This and tuple5 are adapted from Bucklescript - see tuple4 for the original *)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"

let tuple5 decodeA decodeB decodeC decodeD decodeE json =
  if Js.Array.isArray json
  then
    let source : Js.Json.t array = Obj.magic (json : Js.Json.t) in
    let length = Js.Array.length source in
    if length = 5
    then
      try
        ( decodeA (unsafe_get source 0)
        , decodeB (unsafe_get source 1)
        , decodeC (unsafe_get source 2)
        , decodeD (unsafe_get source 3)
        , decodeE (unsafe_get source 4) )
      with DecodeError msg -> raise @@ DecodeError (msg ^ "\n\tin tuple5")
    else
      raise
      @@ DecodeError
           {j|Expected array of length 5, got array of length $length|j}
  else raise @@ DecodeError "Expected array, got not-an-array"


(* external jsGetFluidSelectionRange :
  unit -> int array Js.Nullable.t
  = "getFluidSelectionRange"
  [@@bs.val] [@@bs.scope "window"] *)

(* XXX(JULIAN): All of this should be cleaned up and moved somewhere nice! *)
type jsArrayBuffer = {byteLength : int} [@@bs.deriving abstract]

type jsUint8Array [@@bs.deriving abstract]

external createUint8Array : jsArrayBuffer -> jsUint8Array = "Uint8Array"
  [@@bs.new]

external getUint8ArrayIdx : jsUint8Array -> int -> int = "" [@@bs.get_index]

external setUint8ArrayIdx : jsUint8Array -> int -> int -> unit = ""
  [@@bs.set_index]

(* Note: unsafe. Wrap in bytes_from_base64url, which validates the input *)
let dark_arrayBuffer_from_b64url =
  [%raw
    {|
  function (base64) {
    // Modified version of https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js
    // Note that this version uses the url and filename safe alphabet instead of the standard b64 alphabet.
    // TODO(JULIAN): Figure out how to hoist the `lookup` definition out of the function,
    // since it's shared and could be cached.
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    // Use a lookup table to find the index.
    var lookup = new Uint8Array(256);
    for (var i = 0; i < chars.length; i++) {
      lookup[chars.charCodeAt(i)] = i;
    }


    var bufferLength = base64.length * 0.75, len = base64.length, i, p = 0, encoded1, encoded2, encoded3, encoded4;

    if (base64[base64.length - 1] === "=") {
      bufferLength--;
      if (base64[base64.length - 2] === "=") {
        bufferLength--;
      }
    }

    var arraybuffer = new ArrayBuffer(bufferLength),
    bytes = new Uint8Array(arraybuffer);

    for (i = 0; i < len; i+=4) {
      encoded1 = lookup[base64.charCodeAt(i)];
      encoded2 = lookup[base64.charCodeAt(i+1)];
      encoded3 = lookup[base64.charCodeAt(i+2)];
      encoded4 = lookup[base64.charCodeAt(i+3)];

      bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
      bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
      bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
    }

    return arraybuffer;
  }
|}]


let _bytes_from_uint8Array (input : jsArrayBuffer) : Bytes.t =
  let len = byteLengthGet input in
  let bytes = Bytes.create len in
  let reader = createUint8Array input in
  for i = 0 to len - 1 do
    let char = getUint8ArrayIdx reader i in
    Bytes.unsafe_set bytes i (char_of_int char)
  done ;
  bytes


exception Invalid_B64 of string

let valid_rfc4648_b64_or_exn (str : string) =
  let rfc4648_section5_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\\-_"
  in
  (* '=' isn't in the alphabet, but we allow it as padding *)
  if Util.Regex.exactly ~re:("[" ^ rfc4648_section5_alphabet ^ "=" ^ "]*") str
  then str
  else raise (Invalid_B64 "Expected B64 input matching RFC4648 alphabet.")


let bytes_from_base64url (b64 : string) : Bytes.t =
  b64
  |> valid_rfc4648_b64_or_exn
  |> dark_arrayBuffer_from_b64url
  |> _bytes_from_uint8Array


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
    ; ("Partial", variant2 (fun id _ -> Blank id) id string) ]


let rec tipe j : tipe =
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
    ; ("TDbList", dv1 (fun x -> TDbList x) tipe)
    ; ("TUserType", dv2 (fun n v -> TUserType (n, v)) string int) ]
    j


let traceID j : traceID = wireIdentifier j

let jsDate j : Js.Date.t = Js.Date.fromString (string j)

let rec pattern j : OldExpr.pattern = blankOr nPattern j

and nPattern j : OldExpr.nPattern =
  variants
    [ ("PVariable", variant1 (fun a -> OldExpr.PVariable a) string)
    ; ("PLiteral", variant1 (fun a -> OldExpr.PLiteral a) string)
    ; ( "PConstructor"
      , variant2 (fun a b -> OldExpr.PConstructor (a, b)) string (list pattern)
      ) ]
    j


let rec expr j : OldExpr.expr =
  let blankOrExpr =
    variants
      [ ("Filled", variant2 (fun id v -> F (id, v)) id nExpr)
      ; ("Blank", variant1 (fun id -> Blank id) id) (* We're phasing this out *)
      ; ( "Partial"
        , variant2
            (fun id name ->
              F (id, OldExpr.FluidPartial (name, BlankOr.new_ ())))
            id
            string ) ]
  in
  match blankOrExpr j with
  | F (ID id, FnCall (F (ID "fncall", name), exprs, rail)) ->
      F (ID id, FnCall (F (ID (id ^ "_name"), name), exprs, rail))
  | other ->
      other


and nExpr j : OldExpr.nExpr =
  let open OldExpr in
  let de = expr in
  let dv4 = variant4 in
  let dv3 = variant3 in
  let dv2 = variant2 in
  let dv1 = variant1 in
  variants
    [ ("Let", dv3 (fun a b c -> Let (a, b, c)) (blankOr string) de de)
    ; ("Value", dv1 (fun x -> Value x) string)
    ; ("If", dv3 (fun a b c -> If (a, b, c)) de de de)
    ; ( "FnCall"
      , dv2 (fun a b -> FnCall (F (ID "fncall", a), b, NoRail)) string (list de)
      )
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
      , dv2 (fun a b -> Constructor (a, b)) (blankOr string) (list de) )
    ; ("FluidPartial", dv2 (fun a b -> FluidPartial (a, b)) string de)
    ; ("FluidRightPartial", dv2 (fun a b -> FluidRightPartial (a, b)) string de)
    ]
    j


let fluidExpr (j : Js.Json.t) : fluidExpr = expr j |> OldExpr.toFluidExpr

let blankOrData j : blankOrData =
  let dv1 = variant1 in
  variants
    [ ("PEventName", dv1 (fun x -> PEventName x) (blankOr string))
    ; ("PEventSpace", dv1 (fun x -> PEventSpace x) (blankOr string))
    ; ("PEventModifier", dv1 (fun x -> PEventModifier x) (blankOr string))
    ; ("PDBName", dv1 (fun x -> PDBName x) (blankOr string))
    ; ("PDBColName", dv1 (fun x -> PDBColName x) (blankOr string))
    ; ("PDBColType", dv1 (fun x -> PDBColType x) (blankOr string))
    ; ("PFnName", dv1 (fun x -> PFnName x) (blankOr string))
    ; ("PParamName", dv1 (fun x -> PParamName x) (blankOr string))
    ; ("PParamTipe", dv1 (fun x -> PParamTipe x) (blankOr tipe))
    ; ("PTypeFieldName", dv1 (fun x -> PTypeFieldName x) (blankOr string))
    ; ("PTypeFieldTipe", dv1 (fun x -> PTypeFieldTipe x) (blankOr tipe)) ]
    j


let rec dval j : dval =
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
  let srcT =
    variants
      [ ("SourceNone", dv0 SourceNone)
      ; ("SourceId", dv1 (fun x -> SourceId x) id) ]
  in
  variants
    [ ("DInt", dv1 (fun x -> DInt x) int)
    ; ("DFloat", dv1 (fun x -> DFloat x) Json.Decode.float)
    ; ("DBool", dv1 (fun x -> DBool x) bool)
    ; ("DNull", dv0 DNull)
    ; ("DCharacter", dv1 (fun x -> DCharacter x) string)
    ; ("DStr", dv1 (fun x -> DStr x) string)
    ; ("DList", dv1 (fun x -> DList x) (array dd))
    ; ("DObj", dv1 (fun x -> DObj x) (strDict dd))
    ; ( "DIncomplete"
        (* catch decoding errors for backwards compatibility. if you see this
         * comment in master, the withDefault can be removed *)
      , withDefault (DIncomplete SourceNone) (dv1 (fun x -> DIncomplete x) srcT)
      )
    ; ( "DError"
      , tryDecode2
          (dv1 (fun x -> DError (SourceNone, x)) string)
          (dv1 (fun (i, msg) -> DError (i, msg)) (tuple2 srcT string)) )
    ; ( "DBlock"
      , dv2 (fun x y -> DBlock (x, y)) (list (pair id string)) fluidExpr )
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
            let x = x |> bytes_from_base64url in
            DBytes x)
          string ) ]
    j


and handlerState j : handlerState =
  j
  |> variants
       [ ("HandlerExpanded", variant0 HandlerExpanded)
       ; ("HandlerPrepCollapse", variant0 HandlerPrepCollapse)
       ; ("HandlerCollapsing", variant0 HandlerCollapsing)
       ; ("HandlerCollapsed", variant0 HandlerCollapsed)
       ; ("HandlerExpanding", variant0 HandlerExpanding) ]


and exeState j : exeState =
  j
  |> variants
       [ ("Idle", variant0 Idle)
       ; ("Executing", variant0 Executing)
       ; ("Complete", variant0 Complete) ]


and handlerProp j : handlerProp =
  { handlerLock = field "handlerLock" bool j
  ; handlerState = field "handlerState" handlerState j
  ; hoveringReferences = field "hoveringReferences" (list id) j
  ; execution = field "executing" exeState j
  ; showActions = field "showActions" bool j }


and savedSettings (j : Js.Json.t) : savedSettings =
  (* always use withDefault or optional because the field might be missing due
   * to old editors or new fields. *)
  { editorSettings =
      { runTimers =
          withDefault true (field "editorSettings" (field "runTimers" bool)) j
      ; showFluidDebugger =
          withDefault
            false
            (field "editorSettings" (field "showFluidDebugger" bool))
            j }
  ; cursorState = withDefault Deselected (field "cursorState" cursorState) j
  ; routingTableOpenDetails =
      withDefault StrSet.empty (field "routingTableOpenDetails" strSet) j
  ; tlTraceIDs =
      withDefault TLIDDict.empty (field "tlTraceIDs" (strDict traceID)) j
  ; featureFlags =
      withDefault StrDict.empty (field "featureFlags" (strDict bool)) j
  ; handlerProps =
      withDefault StrDict.empty (field "handlerProps" (strDict handlerProp)) j
  ; canvasPos = withDefault Defaults.origin (field "canvasPos" pos) j
  ; lastReload = optional (field "lastReload" jsDate) j
  ; sidebarOpen =
      withDefault
        Defaults.defaultSavedSettings.sidebarOpen
        (field "sidebarOpen" bool)
        j
  ; showTopbar =
      withDefault
        Defaults.defaultSavedSettings.showTopbar
        (field "showTopbar1" bool)
        j }


and cursorState j =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dv4 = variant4 in
  variants
    [ ("Selecting", dv2 (fun a b -> Selecting (a, b)) tlid (optional id))
    ; ("Entering", dv1 (fun a -> Entering a) entering)
    ; ( "Dragging"
      , dv4 (fun a b c d -> Dragging (a, b, c, d)) tlid vPos bool cursorState )
    ; ("Deselected", dv0 Deselected) (* Old value *)
    ; ("SelectingCommand", dv2 (fun a b -> Selecting (a, Some b)) tlid id)
    ; ("FluidEntering", dv1 (fun a -> FluidEntering a) tlid)
    ; ("FluidMouseSelecting", dv1 (fun a -> FluidEntering a) tlid) ]
    j


and entering j =
  let dv1 = variant1 in
  let dv2 = variant2 in
  variants
    [ ("Creating", dv1 (fun x -> Creating x) pos)
    ; ("Filling", dv2 (fun a b -> Filling (a, b)) tlid id) ]
    j


and loadable (decoder : Js.Json.t -> 'a) (j : Js.Json.t) : 'a loadable =
  variants
    [ ("LoadableSuccess", variant1 (fun a -> LoadableSuccess a) decoder)
    ; ("LoadableNotInitialized", variant0 LoadableNotInitialized)
    ; ( "LoadableLoading"
      , variant1 (fun a -> LoadableLoading a) (optional decoder) )
    ; ("LoadableError", variant1 (fun a -> LoadableError a) string) ]
    j


let dvalDict (j : Js.Json.t) : dvalDict = strDict dval j

let lvDict (j : Js.Json.t) : lvDict = strDict dval j

let analysisEnvelope (j : Js.Json.t) : traceID * dvalDict =
  (tuple2 string dvalDict) j


let handlerSpec j : handlerSpec =
  { space = field "module" (blankOr string) j
  ; name = field "name" (blankOr string) j
  ; modifier = field "modifier" (blankOr string) j }


let handler pos j : handler =
  { ast = field "ast" fluidExpr j
  ; spec = field "spec" handlerSpec j
  ; hTLID = field "tlid" tlid j
  ; pos }


let tipeString j : string = map RT.tipe2str tipe j

let dbColList j : dbColumn list =
  list (tuple2 (blankOr string) (blankOr tipeString)) j


let dbmColList j : dbColumn list =
  list (tuple2 (blankOr string) (blankOr string)) j


let dbMigrationState j : dbMigrationState =
  let dv0 = variant0 in
  variants
    [ ("DBMigrationAbandoned", dv0 DBMigrationAbandoned)
    ; ("DBMigrationInitialized", dv0 DBMigrationInitialized) ]
    j


let dbMigration j : dbMigration =
  { startingVersion = field "starting_version" int j
  ; version = field "version" int j
  ; state = field "state" dbMigrationState j
  ; cols = field "cols" dbColList j
  ; rollforward = field "rollforward" fluidExpr j
  ; rollback = field "rollback" fluidExpr j }


let db pos j : db =
  { dbTLID = field "tlid" tlid j
  ; dbName = field "name" (blankOr string) j
  ; cols = field "cols" dbColList j
  ; version = field "version" int j
  ; oldMigrations = field "old_migrations" (list dbMigration) j
  ; activeMigration = field "active_migration" (optional dbMigration) j
  ; pos }


let toplevel j : toplevel =
  let pos = field "pos" pos j in
  let variant =
    variants
      [ ("Handler", variant1 (fun x -> TLHandler x) (handler pos))
      ; ("DB", variant1 (fun x -> TLDB x) (db pos)) ]
  in
  field "data" variant j


let userFunctionParameter j : userFunctionParameter =
  { ufpName = field "name" (blankOr string) j
  ; ufpTipe = field "tipe" (blankOr tipe) j
  ; ufpBlock_args = field "block_args" (list string) j
  ; ufpOptional = field "optional" bool j
  ; ufpDescription = field "description" string j }


let userFunctionMetadata j : userFunctionMetadata =
  { ufmName = field "name" (blankOr string) j
  ; ufmParameters = field "parameters" (list userFunctionParameter) j
  ; ufmDescription = field "description" string j
  ; ufmReturnTipe = field "return_type" (blankOr tipe) j
  ; ufmInfix = field "infix" bool j }


let userFunction j : userFunction =
  { ufTLID = field "tlid" tlid j
  ; ufMetadata = field "metadata" userFunctionMetadata j
  ; ufAST = field "ast" expr j |> OldExpr.toFluidExpr }


let fof j : fourOhFour =
  { space = index 0 string j
  ; path = index 1 string j
  ; modifier = index 2 string j
  ; timestamp = index 3 string j
  ; traceID = index 4 traceID j }


let deployStatus j : deployStatus =
  let sumtypes =
    [("Deployed", variant0 Deployed); ("Deploying", variant0 Deploying)]
  in
  j |> variants sumtypes


let sDeploy j : staticDeploy =
  { deployHash = field "deploy_hash" string j
  ; url = field "url" string j
  ; lastUpdate = field "last_update" jsDate j
  ; status = field "status" deployStatus j }


let serverTime j : Js.Date.t = Js.Date.fromString (field "value" string j)

let presenceMsg j : avatar =
  { canvasId = field "canvasId" string j
  ; canvasName = field "canvasName" string j
  ; tlid = field "tlid" (optional string) j
  ; username = field "username" string j
  ; serverTime = field "serverTime" serverTime j
  ; email = field "email" string j
  ; fullname = field "name" (optional string) j
  ; browserId = field "browserId" string j }


let inputValueDict j : inputValueDict =
  j |> list (tuple2 string dval) |> StrDict.fromList


let functionResult j : functionResult =
  let fnName, callerID, argHash, argHashVersion, value =
    tuple5 string id string int dval j
  in
  {fnName; callerID; argHash; argHashVersion; value}


let traceData j : traceData =
  { input = field "input" inputValueDict j
  ; timestamp = field "timestamp" string j
  ; functionResults = field "function_results" (list functionResult) j }


let trace j : trace = pair traceID (optional traceData) j

let traces j : traces =
  j |> list (tuple2 wireIdentifier (list trace)) |> StrDict.fromList


let userRecordField j =
  { urfName = field "name" (blankOr string) j
  ; urfTipe = field "tipe" (blankOr tipe) j }


let userTipeDefinition j =
  variants
    [("UTRecord", variant1 (fun x -> UTRecord x) (list userRecordField))]
    j


let userTipe j =
  { utTLID = field "tlid" tlid j
  ; utName = field "name" (blankOr string) j
  ; utVersion = field "version" int j
  ; utDefinition = field "definition" userTipeDefinition j }


let permission j =
  variants [("Read", variant0 Read); ("ReadWrite", variant0 ReadWrite)] j


let op j : op =
  variants
    [ ( "SetHandler"
      , variant3
          (fun t p h -> SetHandler (t, p, {h with pos = p}))
          tlid
          pos
          (handler {x = -1286; y = -467}) )
    ; ( "CreateDB"
      , variant3 (fun t p name -> CreateDB (t, p, name)) tlid pos string )
    ; ("AddDBCol", variant3 (fun t cn ct -> AddDBCol (t, cn, ct)) tlid id id)
    ; ( "SetDBColName"
      , variant3 (fun t i name -> SetDBColName (t, i, name)) tlid id string )
    ; ( "ChangeDBColName"
      , variant3 (fun t i name -> ChangeDBColName (t, i, name)) tlid id string
      )
    ; ( "SetDBColType"
      , variant3 (fun t i tipe -> SetDBColType (t, i, tipe)) tlid id string )
    ; ( "ChangeDBColType"
      , variant3 (fun t i tipe -> ChangeDBColName (t, i, tipe)) tlid id string
      )
    ; ("DeleteDBCol", variant2 (fun t i -> DeleteDBCol (t, i)) tlid id)
      (* deprecated, can't happen *)
    ; ("DeprecatedInitDbm", variant1 (fun _ -> UndoTL (TLID "")) tlid)
    ; ( "CreateDBMigration"
      , variant4
          (fun t rbid rfid cols -> CreateDBMigration (t, rbid, rfid, cols))
          tlid
          id
          id
          dbmColList )
    ; ( "AddDBColToDBMigration"
      , variant3
          (fun t colnameid coltypeid ->
            AddDBColToDBMigration (t, colnameid, coltypeid))
          tlid
          id
          id )
    ; ( "SetDBColNameInDBMigration"
      , variant3
          (fun t i name -> SetDBColNameInDBMigration (t, i, name))
          tlid
          id
          string )
    ; ( "SetDBColTypeInDBMigration"
      , variant3
          (fun t i tipe -> SetDBColTypeInDBMigration (t, i, tipe))
          tlid
          id
          string )
    ; ("AbandonDBMigration", variant1 (fun t -> AbandonDBMigration t) tlid)
    ; ( "DeleteColInDBMigration"
      , variant2 (fun t i -> DeleteColInDBMigration (t, i)) tlid id )
    ; ("TLSavepoint", variant1 (fun t -> TLSavepoint t) tlid)
    ; ("UndoTL", variant1 (fun t -> UndoTL t) tlid)
    ; ("RedoTL", variant1 (fun t -> RedoTL t) tlid)
    ; ("DeleteTL", variant1 (fun t -> DeleteTL t) tlid)
    ; ("MoveTL", variant2 (fun t p -> MoveTL (t, p)) tlid pos)
    ; ("SetFunction", variant1 (fun uf -> SetFunction uf) userFunction)
    ; ("DeleteFunction", variant1 (fun t -> DeleteFunction t) tlid)
    ; ( "SetExpr"
      , variant3
          (fun t i e -> SetExpr (t, i, OldExpr.toFluidExpr e))
          tlid
          id
          expr )
    ; ( "RenameDBname"
      , variant2 (fun t name -> RenameDBname (t, name)) tlid string )
    ; ( "CreateDBWithBlankOr"
      , variant4
          (fun t p i name -> CreateDBWithBlankOr (t, p, i, name))
          tlid
          pos
          id
          string )
    ; ("DeleteFunctionForever", variant1 (fun t -> DeleteFunctionForever t) tlid)
    ; ("DeleteTLForever", variant1 (fun t -> DeleteTLForever t) tlid)
    ; ("SetType", variant1 (fun t -> SetType t) userTipe)
    ; ("DeleteType", variant1 (fun t -> DeleteType t) tlid)
    ; ("DeleteTypeForever", variant1 (fun t -> DeleteTypeForever t) tlid) ]
    j


let addOpAPIResult j : addOpAPIResult =
  let tls = field "toplevels" (list toplevel) j in
  let dtls = field "deleted_toplevels" (list toplevel) j in
  { handlers = List.filterMap ~f:TL.asHandler tls
  ; deletedHandlers = List.filterMap ~f:TL.asHandler dtls
  ; dbs = List.filterMap ~f:TL.asDB tls
  ; deletedDBs = List.filterMap ~f:TL.asDB dtls
  ; userFunctions = field "user_functions" (list userFunction) j
  ; deletedUserFunctions = field "deleted_user_functions" (list userFunction) j
  ; userTipes = field "user_tipes" (list userTipe) j
  ; deletedUserTipes = field "deleted_user_tipes" (list userTipe) j }


let addOpAPIParams j : addOpAPIParams =
  (* if we roll back the server, we might get new client code (this code), but
   * no opCtr from the server, so handle that case *)
  let opCtr = try Some (field "opCtr" int j) with _ -> None in
  { ops = field "ops" (list op) j
  ; opCtr
    (* withDefault in case we roll back and have an old server that doesn't send
* us this field *)
  ; clientOpCtrId = withDefault "" (field "clientOpCtrId" string) j }


let addOpAPIStrollerMsg j : addOpStrollerMsg =
  { result = field "result" addOpAPIResult j
  ; params = field "params" addOpAPIParams j }


let getUnlockedDBsAPIResult j : getUnlockedDBsAPIResult =
  j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList


let getTraceDataAPIResult j : getTraceDataAPIResult =
  {trace = field "trace" trace j}


let dbStats j : dbStats =
  { count = field "count" int j
  ; example = field "example" (optional (tuple2 dval string)) j }


let dbStatsStore j : dbStatsStore = strDict dbStats j

let dbStatsAPIResult j = dbStatsStore j

let account j : account =
  { name = field "name" string j
  ; email = field "email" string j
  ; username = field "username" string j }


(* schedule is None here but gets updated when we create a view state
 * see createVS in ViewUtils.ml for details *)
let workerStats j : workerStats = {count = field "count" int j; schedule = None}

let workerStatsAPIResult j = workerStats j

let updateWorkerScheduleAPIResult j : string StrDict.t = (strDict string) j

let initialLoadAPIResult j : initialLoadAPIResult =
  let tls = field "toplevels" (list toplevel) j in
  let dtls = field "deleted_toplevels" (list toplevel) j in
  { handlers = List.filterMap ~f:TL.asHandler tls
  ; deletedHandlers = List.filterMap ~f:TL.asHandler dtls
  ; dbs = List.filterMap ~f:TL.asDB tls
  ; deletedDBs = List.filterMap ~f:TL.asDB dtls
  ; userFunctions = field "user_functions" (list userFunction) j
  ; deletedUserFunctions = field "deleted_user_functions" (list userFunction) j
  ; unlockedDBs =
      j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList
  ; fofs = field "fofs" (list fof) j
  ; staticDeploys = field "assets" (list sDeploy) j
  ; traces = field "traces" (list (pair tlid traceID)) j
  ; userTipes = field "user_tipes" (list userTipe) j
  ; deletedUserTipes = field "deleted_user_tipes" (list userTipe) j
  ; opCtrs =
      j
      |> withDefault [] (field "op_ctrs" (list (tuple2 string int)))
      |> StrDict.fromList
  ; permission = field "permission" (optional permission) j
  ; groups = List.filterMap ~f:TL.asGroup tls
  ; deletedGroups = List.filterMap ~f:TL.asGroup tls
  ; account = field "account" account j
  ; worker_schedules = field "worker_schedules" (strDict string) j }


let executeFunctionAPIResult j : executeFunctionAPIResult =
  ( field "result" dval j
  , field "hash" string j
  , field "hashVersion" int j
  , field "touched_tlids" (list tlid) j
  , j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList )


let triggerHandlerAPIResult j : triggerHandlerAPIResult =
  field "touched_tlids" (list tlid) j


let saveTestAPIResult j : saveTestAPIResult = string j

(* -------------------------- *)
(* Dval (some here because of cyclic dependencies) *)
(* ------------------------- *)

let parseBasicDval str : dval =
  oneOf
    [ map (fun x -> DInt x) int
    ; map (fun x -> DFloat x) Json.Decode.float
    ; map (fun x -> DBool x) bool
    ; nullAs DNull
    ; map (fun x -> DStr x) string ]
    str


(* Ported directly from Dval.parse in the backend *)
let parseDvalLiteral (str : string) : dval option =
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


let exception_ j : exception_ =
  { short = field "short" string j
  ; long = field "long" (optional string) j
  ; exceptionTipe = field "tipe" string j
  ; actual = field "actual" (optional string) j
  ; actualType = field "actual_tipe" (optional string) j
  ; expected = field "expected" (optional string) j
  ; result = field "result" (optional string) j
  ; resultType = field "result_tipe" (optional string) j
  ; info = field "info" (strDict string) j
  ; workarounds = field "workarounds" (list string) j }


(* Wrap JSON decoders using bs-json's format, into TEA's HTTP expectation format *)
let wrapExpect (fn : Js.Json.t -> 'a) : string -> ('ok, string) Tea.Result.t =
 fun j ->
  try Ok (fn (Json.parseOrRaise j))
  with e ->
    reportError "unexpected json" j ;
    ( match e with
    | DecodeError e | Json.ParseError e ->
        Error e
    | e ->
        Error (Printexc.to_string e) )


(* Wrap JSON decoders using bs-json's format, into TEA's JSON decoder format *)
let wrapDecoder (fn : Js.Json.t -> 'a) : (Js.Json.t, 'a) Tea.Json.Decoder.t =
  Decoder
    (fun value ->
      try Tea_result.Ok (fn value)
      with e ->
        reportError "undecodable json" value ;
        ( match e with
        | DecodeError e | Json.ParseError e ->
            Tea_result.Error e
        | e ->
            Tea_result.Error ("Json error: " ^ Printexc.to_string e) ))
