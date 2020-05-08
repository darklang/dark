open Prelude
open Json.Decode

(* Dark *)
module TL = Toplevel
module RT = Runtime

type id = Shared.id

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


(* IDs are strings in the client. The server serializes IDs to ints, while the
 * client serializes them to strings, so they could actually be either.
 *
 * This is actually a really important path for responsiveness of the client.
 * In the past we used tried one decoder, then the other, using an exception.
 * This is very very slow. It's possible that testing it isn't the fastest
 * approach, but it no longer appears in profiles, so it's at least good
 * enough. If you change this, profile the changes (expression decoding) to
 * ensure it's still fast.
 *
 * We should change the formats so that we always know what we're getting. *)
let wireIdentifier (j : Js.Json.t) =
  if Js.typeof j = "string" then string j else string_of_int (Obj.magic j : int)


let id = ID.fromString << wireIdentifier

let tlid = TLID.fromString << wireIdentifier

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
    ; ("TCharacter", dv0 TCharacter)
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
    ; ("TDbList", dv1 (fun x -> TDbList x) tipe)
    ; ("TPassword", dv0 TPassword)
    ; ("TUuid", dv0 TUuid)
    ; ("TOption", dv0 TOption)
    ; ("TErrorRail", dv0 TErrorRail)
    ; ("TResult", dv0 TResult)
    ; ("TUserType", dv2 (fun n v -> TUserType (n, v)) string int)
    ; ("TBytes", dv0 TBytes) ]
    j


let traceID j : traceID = wireIdentifier j

let jsDate j : Js.Date.t = Js.Date.fromString (string j)

let sendToRail j =
  let dv0 = variant0 in
  variants
    [("Rail", dv0 FluidExpression.Rail); ("NoRail", dv0 FluidExpression.NoRail)]
    j


let rec fluidPattern j : FluidPattern.t =
  let module P = FluidPattern in
  let dp = fluidPattern in
  let dv4 = variant4 in
  let dv3 = variant3 in
  let dv2 = variant2 in
  variants
    [ ("FPVariable", dv3 (fun a b c -> P.FPVariable (a, b, c)) id id string)
    ; ( "FPConstructor"
      , dv4 (fun a b c d -> P.FPConstructor (a, b, c, d)) id id string (list dp)
      )
    ; ("FPInteger", dv3 (fun a b c -> P.FPInteger (a, b, c)) id id string)
    ; ("FPBool", dv3 (fun a b c -> P.FPBool (a, b, c)) id id bool)
    ; ( "FPString"
      , recordVariant3
          (fun matchID patternID str -> P.FPString {matchID; patternID; str})
          ("matchID", id)
          ("patternID", id)
          ("str", string) )
    ; ( "FPFloat"
      , dv4 (fun a b c d -> P.FPFloat (a, b, c, d)) id id string string )
    ; ("FPNull", dv2 (fun a b -> P.FPNull (a, b)) id id)
    ; ("FPBlank", dv2 (fun a b -> P.FPBlank (a, b)) id id) ]
    j


let rec fluidExpr (j : Js.Json.t) : FluidExpression.t =
  let module E = FluidExpression in
  let de = fluidExpr in
  let dv5 = variant5 in
  let dv4 = variant4 in
  let dv3 = variant3 in
  let dv2 = variant2 in
  let dv1 = variant1 in
  variants
    [ ("EInteger", dv2 (fun x y -> E.EInteger (x, y)) id string)
    ; ("EBool", dv2 (fun x y -> E.EBool (x, y)) id bool)
    ; ("EString", dv2 (fun x y -> E.EString (x, y)) id string)
    ; ("EFloat", dv3 (fun x y z -> E.EFloat (x, y, z)) id string string)
    ; ("ENull", dv1 (fun x -> E.ENull x) id)
    ; ("EBlank", dv1 (fun x -> E.EBlank x) id)
    ; ("ELet", dv4 (fun a b c d -> E.ELet (a, b, c, d)) id string de de)
    ; ("EIf", dv4 (fun a b c d -> E.EIf (a, b, c, d)) id de de de)
    ; ( "EBinOp"
      , dv5
          (fun a b c d e -> E.EBinOp (a, b, c, d, e))
          id
          string
          de
          de
          sendToRail )
    ; ( "ELambda"
      , dv3 (fun a b c -> E.ELambda (a, b, c)) id (list (pair id string)) de )
    ; ("EFieldAccess", dv3 (fun a b c -> E.EFieldAccess (a, b, c)) id de string)
    ; ("EVariable", dv2 (fun x y -> E.EVariable (x, y)) id string)
    ; ( "EFnCall"
      , dv4
          (fun a b c d -> E.EFnCall (a, b, c, d))
          id
          string
          (list de)
          sendToRail )
    ; ("EPartial", dv3 (fun a b c -> E.EPartial (a, b, c)) id string de)
    ; ("ELeftPartial", dv3 (fun a b c -> E.ELeftPartial (a, b, c)) id string de)
    ; ( "ERightPartial"
      , dv3 (fun a b c -> E.ERightPartial (a, b, c)) id string de )
    ; ("EList", dv2 (fun x y -> E.EList (x, y)) id (list de))
    ; ("ERecord", dv2 (fun x y -> E.ERecord (x, y)) id (list (pair string de)))
    ; ("EPipe", dv2 (fun x y -> E.EPipe (x, y)) id (list de))
    ; ( "EConstructor"
      , dv3 (fun a b c -> E.EConstructor (a, b, c)) id string (list de) )
    ; ( "EMatch"
      , dv3
          (fun a b c -> E.EMatch (a, b, c))
          id
          de
          (list (pair fluidPattern de)) )
    ; ("EPipeTarget", dv1 (fun a -> E.EPipeTarget a) id)
    ; ( "EFeatureFlag"
      , dv5 (fun a b c d e -> E.EFeatureFlag (a, b, c, d, e)) id string de de de
      ) ]
    j


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
      ; ("SourceId", dv2 (fun x y -> SourceId (x, y)) tlid id) ]
  in
  let dblock_args j =
    { params = field "params" (list (pair id string)) j
    ; body = field "body" fluidExpr j
    ; symtable = field "symtable" (strDict dval) j }
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
    ; ("DBlock", dv1 (fun x -> DBlock x) dblock_args)
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
  ; execution = field "executing" exeState j }


and savedUserSettings (j : Js.Json.t) : savedUserSettings =
  { showUserWelcomeModal =
      withDefault
        Defaults.defaultUserSettings.showUserWelcomeModal
        (field "showUserWelcomeModal" bool)
        j
  ; recordConsent = withDefault None (field "recordConsent" (optional bool)) j
  }


and sidebarMode (j : Js.Json.t) : sidebarMode =
  j
  |> variants
       [ ("DetailedMode", variant0 DetailedMode)
       ; ("AbridgedMode", variant0 AbridgedMode) ]


and sidebarState (j : Js.Json.t) : sidebarState =
  { mode = field "mode" sidebarMode j
  ; openedCategories =
      withDefault StrSet.empty (field "openedCategories" strSet) j }


and savedSettings (j : Js.Json.t) : savedSettings =
  (* always use withDefault or optional because the field might be missing due
   * to old editors or new fields. *)
  { editorSettings =
      { runTimers =
          withDefault true (field "editorSettings" (field "runTimers" bool)) j
      ; showHandlerASTs =
          withDefault
            false
            (field "editorSettings" (field "showHandlerASTs" bool))
            j
      ; showFluidDebugger =
          withDefault
            false
            (field "editorSettings" (field "showFluidDebugger" bool))
            j }
  ; cursorState = withDefault Deselected (field "cursorState" cursorState) j
  ; tlTraceIDs =
      withDefault TLIDDict.empty (field "tlTraceIDs" (strDict traceID)) j
  ; featureFlags =
      withDefault StrDict.empty (field "featureFlags" (strDict bool)) j
  ; handlerProps =
      withDefault StrDict.empty (field "handlerProps" (strDict handlerProp)) j
  ; canvasPos = withDefault Defaults.origin (field "canvasPos" pos) j
  ; lastReload = optional (field "lastReload" jsDate) j
  ; sidebarState =
      withDefault Defaults.defaultSidebar (field "sidebarState" sidebarState) j
  ; showTopbar =
      withDefault
        Defaults.defaultSavedSettings.showTopbar
        (field "showTopbar1" bool)
        j
  ; firstVisitToCanvas =
      withDefault
        Defaults.defaultSavedSettings.firstVisitToCanvas
        (field "firstVisitToCanvas" bool)
        j }


and cursorState j =
  let dv0 = variant0 in
  let dv1 = variant1 in
  let dv2 = variant2 in
  let dv3 = variant3 in
  let dv4 = variant4 in
  variants
    [ ("Selecting", dv2 (fun a b -> Selecting (a, b)) tlid (optional id))
    ; ("Entering", dv1 (fun a -> Entering a) entering)
    ; ( "Dragging" (* Deprecated via DraggingTL *)
      , dv4 (fun a b c d -> DraggingTL (a, b, c, d)) tlid vPos bool cursorState
      )
    ; ( "DraggingTL"
      , dv4 (fun a b c d -> DraggingTL (a, b, c, d)) tlid vPos bool cursorState
      )
    ; ( "PanningCanvas"
        (* TODO: There's a danger of mismatching the encoder order here because we're using an inline record.
         * An order-independent encoding would alleviate this. *)
      , dv3
          (fun viewportStart viewportCurr prevCursorState ->
            PanningCanvas {viewportStart; viewportCurr; prevCursorState})
          vPos
          vPos
          cursorState )
    ; ("Deselected", dv0 Deselected) (* Old value *)
    ; ("SelectingCommand", dv2 (fun a b -> Selecting (a, Some b)) tlid id)
    ; ("FluidEntering", dv1 (fun a -> FluidEntering a) tlid)
    ; ("FluidMouseSelecting", dv1 (fun a -> FluidEntering a) tlid) ]
    j


and entering j =
  let dv1 = variant1 in
  let dv2 = variant2 in
  variants
    [ ( "Creating"
      , dv1
          (fun x -> Creating (if x = Defaults.origin then None else Some x))
          pos )
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


let executionResult (j : Js.Json.t) : executionResult =
  variants
    [ ("ExecutedResult", variant1 (fun a -> ExecutedResult a) dval)
    ; ("NonExecutedResult", variant1 (fun a -> NonExecutedResult a) dval) ]
    j


let intermediateResultStore (j : Js.Json.t) : intermediateResultStore =
  strDict executionResult j


let dvalDict (j : Js.Json.t) : dvalDict = strDict dval j

let analysisEnvelope (j : Js.Json.t) : traceID * intermediateResultStore =
  (tuple2 string intermediateResultStore) j


let handlerSpec j : handlerSpec =
  { space = field "module" (blankOr string) j
  ; name = field "name" (blankOr string) j
  ; modifier = field "modifier" (blankOr string) j }


let handler pos j : handler =
  { ast = field "ast" (fun j -> fluidExpr j |> FluidAST.ofExpr) j
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
  ; ufAST = field "ast" fluidExpr j |> FluidAST.ofExpr }


let packageFnParameter (j : Js.Json.t) : Types.packageFnParameter =
  { name = field "name" string j
  ; tipe = field "tipe" tipe j
  ; description = field "description" string j }


let packageFn (j : Js.Json.t) : Types.packageFn =
  { user = field "user" string j
  ; package = field "package" string j
  ; module_ = field "module" string j
  ; fnname = field "fnname" string j
  ; version = field "version" int j
  ; body = field "body" fluidExpr j
  ; parameters = field "parameters" (list packageFnParameter) j
  ; return_type = field "return_type" tipe j
  ; description = field "description" string j
  ; author = field "author" string j
  ; deprecated = field "deprecated" bool j
  ; pfTLID = field "tlid" tlid j }


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


let trace j : trace =
  pair traceID (optional traceData) j
  |> fun (id, traceData) ->
  match traceData with
  | None ->
      (id, Result.fail NoneYet)
  | Some traceData ->
      (id, Result.succeed traceData)


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
    ; ("DeprecatedInitDbm", variant1 (fun _ -> UndoTL TLID.empty) tlid)
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
    ; ("SetExpr", variant3 (fun t i e -> SetExpr (t, i, e)) tlid id fluidExpr)
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


let addOpAPI (j : Js.Json.t) : addOpAPIResponse =
  {result = field "result" addOpAPIResult j}


let addOpAPIParams j : addOpAPIParams =
  { ops = field "ops" (list op) j
  ; opCtr = field "opCtr" int j
  ; clientOpCtrId = (field "clientOpCtrId" string) j }


let addOpAPIStrollerMsg (j : Js.Json.t) : addOpStrollerMsg =
  { result = field "result" addOpAPIResult j
  ; params = field "params" addOpAPIParams j }


let getUnlockedDBsAPIResult j : getUnlockedDBsAPIResult =
  j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList


let get404sAPIResult j : get404sAPIResult = j |> field "f404s" (list fof)

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
  ; staticDeploys = field "assets" (list sDeploy) j
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
  ; canvasList = field "canvas_list" (list string) j
  ; orgs = field "orgs" (list string) j
  ; orgCanvasList = field "org_canvas_list" (list string) j
  ; workerSchedules = field "worker_schedules" (strDict string) j
  ; creationDate = field "creation_date" jsDate j }


let allTracesResult j : allTracesAPIResult =
  {traces = field "traces" (list (pair tlid traceID)) j}


let executeFunctionAPIResult j : executeFunctionAPIResult =
  ( field "result" dval j
  , field "hash" string j
  , field "hashVersion" int j
  , field "touched_tlids" (list tlid) j
  , j |> field "unlocked_dbs" (list wireIdentifier) |> StrSet.fromList )


let uploadFnAPIResult _ : uploadFnAPIResult = ()

let loadPackagesAPIResult j : loadPackagesAPIResult = list packageFn j

let loadCanvasInfoAPIResult j : SettingsViewTypes.loadCanvasInfoAPIResult =
  { canvasDescription = field "canvasDescription" string j
  ; shippedDate = field "canvasShippedDate" string j }


let triggerHandlerAPIResult j : triggerHandlerAPIResult =
  field "touched_tlids" (list tlid) j


let saveTestAPIResult j : saveTestAPIResult = string j

let optBool j : bool option = optional bool j

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


(** [clickEvent fn] implements a decoder converting a javascript mouse event
 * into an OCaml record of type mouseEvent.
 *
 * Example usage:
 *
 *  let constructor = (fun mouseEvent -> AppMouseDown mouseEvent) in
 *  Tea.Html.onWithOptions
 *   ~key
 *   event
 *   {stopPropagation = true; preventDefault = true}
 *   (Decoders.wrapDecoder (Decoders.clickEvent constructor))
 *)
let clickEvent (fn : mouseEvent -> 'a) j : 'a =
  fn
    { mePos =
        (* We decode floats b/c newer Chromes may use floats instead of ints; we
         * then truncate rather than moving to floats everywhere due to concerns
         * about sending data back to browsers whose DOMs don't support float
         * positions - see https://github.com/darklang/dark/pull/2016 for
         * discussion, and
         * https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
         * for the spec *)
        { vx = field "pageX" Json.Decode.float j |> truncate
        ; vy = field "pageY" Json.Decode.float j |> truncate }
    ; button = field "button" int j
    ; ctrlKey = field "ctrlKey" bool j
    ; shiftKey = field "shiftKey" bool j
    ; altKey = field "altKey" bool j
    ; detail = field "detail" int j }


(** [scrollEvent fn] implements a decoder converting a javascript scroll event
 * into an OCaml record of type scrollEvent.
 *
 * Example usage:
 *
 *  let constructor = (fun _scrollEvent -> AppScroll) in
 *  Tea.Html.onWithOptions
 *   ~key
 *   event
 *   {stopPropagation = true; preventDefault = true}
 *   (Decoders.wrapDecoder (Decoders.scrollEvent constructor))
 *)
let scrollEvent (fn : scrollEvent -> 'a) j : 'a =
  fn {timeStamp = field "timeStamp" Json.Decode.float j}


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
