open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

(* ------------------------- *)
(* Toplevel manipulation *)
(* ------------------------- *)
let name (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      "H: " ^ (h.spec.name |> B.toOption |> Option.withDefault ~default:"")
  | TLDB db ->
      "DB: " ^ (db.dbName |> B.toOption |> Option.withDefault ~default:"")
  | TLPmFunc f ->
      "Package Manager Func: " ^ f.fnname
  | TLFunc f ->
      "Func: "
      ^ (f.ufMetadata.ufmName |> B.toOption |> Option.withDefault ~default:"")
  | TLTipe t ->
      "Type: " ^ (t.utName |> B.toOption |> Option.withDefault ~default:"")
  | TLGroup g ->
      "Group: " ^ (g.gName |> B.toOption |> Option.withDefault ~default:"")


let sortkey (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      (h.spec.space |> B.toOption |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.name |> B.toOption |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.modifier |> B.toOption |> Option.withDefault ~default:"")
  | TLDB db ->
      db.dbName |> B.toOption |> Option.withDefault ~default:"Undefined"
  | TLPmFunc f ->
      f.fnname
  | TLFunc f ->
      f.ufMetadata.ufmName |> B.toOption |> Option.withDefault ~default:""
  | TLTipe t ->
      t.utName |> B.toOption |> Option.withDefault ~default:""
  | TLGroup g ->
      g.gName |> B.toOption |> Option.withDefault ~default:""


let id tl =
  match tl with
  | TLHandler h ->
      h.hTLID
  | TLDB db ->
      db.dbTLID
  | TLFunc f ->
      f.ufTLID
  | TLPmFunc f ->
      f.pfTLID
  | TLTipe t ->
      t.utTLID
  | TLGroup g ->
      g.gTLID


let pos tl =
  match tl with
  | TLHandler h ->
      h.pos
  | TLDB db ->
      db.pos
  | TLGroup g ->
      g.pos
  | TLPmFunc f ->
      recover "no pos in a func" ~debug:f.pfTLID {x = 0; y = 0}
  | TLFunc f ->
      recover "no pos in a func" ~debug:f.ufTLID {x = 0; y = 0}
  | TLTipe t ->
      recover "no pos in a tipe" ~debug:t.utTLID {x = 0; y = 0}


let remove (m : model) (tl : toplevel) : model =
  let m = {m with cursorState = Deselected; currentPage = Architecture} in
  match tl with
  | TLHandler h ->
      Handlers.remove m h
  | TLDB db ->
      DB.remove m db
  | TLFunc f ->
      UserFunctions.remove m f
  | TLTipe ut ->
      UserTypes.remove m ut
  | TLGroup g ->
      Groups.remove m g
  | TLPmFunc _ ->
      (* Cannot remove a package manager function *)
      m


let fromList (tls : toplevel list) : toplevel TLIDDict.t =
  tls |> List.map ~f:(fun tl -> (id tl, tl)) |> TD.fromList


let move (tlid : TLID.t) (xOffset : int) (yOffset : int) (m : model) : model =
  let newPos p = {x = p.x + xOffset; y = p.y + yOffset} in
  { m with
    handlers =
      TD.updateIfPresent m.handlers ~tlid ~f:(fun (h : handler) ->
          {h with pos = newPos h.pos})
  ; dbs =
      TD.updateIfPresent m.dbs ~tlid ~f:(fun (db : db) ->
          {db with pos = newPos db.pos})
  ; groups =
      TD.updateIfPresent m.groups ~tlid ~f:(fun (group : group) ->
          {group with pos = newPos group.pos}) }


let ufToTL (uf : userFunction) : toplevel = TLFunc uf

let pmfToTL (pmf : packageFn) : toplevel = TLPmFunc pmf

let utToTL (ut : userTipe) : toplevel = TLTipe ut

let asUserFunction (tl : toplevel) : userFunction option =
  match tl with TLFunc f -> Some f | _ -> None


let asUserTipe (tl : toplevel) : userTipe option =
  match tl with TLTipe t -> Some t | _ -> None


let asGroup (tl : toplevel) : group option =
  match tl with TLGroup g -> Some g | _ -> None


let isUserFunction (tl : toplevel) : bool =
  match tl with TLFunc _ -> true | _ -> false


let isUserTipe (tl : toplevel) : bool =
  match tl with TLTipe _ -> true | _ -> false


let isGroup (tl : toplevel) : bool =
  match tl with TLGroup _ -> true | _ -> false


let asHandler (tl : toplevel) : handler option =
  match tl with TLHandler h -> Some h | _ -> None


let asDB (tl : toplevel) : db option =
  match tl with TLDB h -> Some h | _ -> None


let isDB (tl : toplevel) : bool = match tl with TLDB _ -> true | _ -> false

let isHandler (tl : toplevel) : bool =
  match tl with TLHandler _ -> true | _ -> false


let handlers (tls : toplevel list) : handler list =
  List.filterMap ~f:asHandler tls


let dbs (tls : toplevel TD.t) : db list = tls |> TD.filterMapValues ~f:asDB

let spaceOfHandler (h : handler) : handlerSpace = SpecHeaders.spaceOf h.spec

let spaceOf (tl : toplevel) : handlerSpace option =
  tl |> asHandler |> Option.map ~f:spaceOfHandler


let isHTTPHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSHTTP)

let isCronHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSCron)

let isWorkerHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSWorker)


let isReplHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSRepl)

let isDeprecatedCustomHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSDeprecatedOther)


let toOp (tl : toplevel) : op list =
  match tl with
  | TLHandler h ->
      [SetHandler (h.hTLID, h.pos, h)]
  | TLFunc fn ->
      [SetFunction fn]
  | TLTipe t ->
      [SetType t]
  | TLPmFunc _ ->
      recover "Package Manager functions are not editable" ~debug:(id tl) []
  | TLGroup _ ->
      recover "Groups are front end only" ~debug:(id tl) []
  | TLDB _ ->
      recover "This isn't how datastore ops work" ~debug:(id tl) []


let customEventSpaceNames (handlers : handler TD.t) : string list =
  let otherSpaces =
    handlers
    |> TD.mapValues ~f:(fun h -> TLHandler h)
    |> List.filter ~f:isDeprecatedCustomHandler
    |> List.filterMap ~f:(fun tl ->
           asHandler tl |> Option.andThen ~f:(fun h -> B.toOption h.spec.space))
  in
  otherSpaces


(* ------------------------- *)
(* Generic *)
(* ------------------------- *)
let blankOrData (tl : toplevel) : blankOrData list =
  match tl with
  | TLHandler h ->
      SpecHeaders.blankOrData h.spec
  | TLDB db ->
      DB.blankOrData db
  | TLPmFunc f ->
      PackageManager.blankOrData f
  | TLFunc f ->
      UserFunctions.blankOrData f
  | TLTipe t ->
      UserTypes.blankOrData t
  | TLGroup g ->
      Groups.blankOrData g


let isValidBlankOrID (tl : toplevel) (id : ID.t) : bool =
  List.member ~value:id (tl |> blankOrData |> List.map ~f:P.toID)


(* ------------------------- *)
(* ASTs *)
(* ------------------------- *)

let getAST (tl : toplevel) : FluidAST.t option =
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | TLPmFunc fn ->
      Some (FluidAST.ofExpr fn.body)
  | _ ->
      None


let updateModelWithAST (m : model) (tlid : TLID.t) (ast : FluidAST.t) : model =
  { m with
    handlers = TD.updateIfPresent m.handlers ~tlid ~f:(fun h -> {h with ast})
  ; userFunctions =
      TD.updateIfPresent m.userFunctions ~tlid ~f:(fun uf ->
          {uf with ufAST = ast}) }


let updateModelASTCache (tlid : TLID.t) (str : string) (m : model) : model =
  let searchCache =
    m.searchCache |> TLIDDict.update ~tlid ~f:(fun _ -> Some str)
  in
  {m with searchCache}


(* Create the modification to set the AST in this toplevel. `ops` is optional
 * other ops to include in this modification. Does not change the model. *)
let setASTOpMod ?(ops = []) (tl : toplevel) (ast : FluidAST.t) : modification =
  match tl with
  | TLHandler h ->
      if h.ast = ast
      then NoChange
      else
        AddOps (ops @ [SetHandler (id tl, h.pos, {h with ast})], FocusNoChange)
  | TLFunc f ->
      if f.ufAST = ast
      then NoChange
      else AddOps (ops @ [SetFunction {f with ufAST = ast}], FocusNoChange)
  | TLPmFunc _ ->
      recover "cannot change ast in package manager" ~debug:tl NoChange
  | TLTipe _ ->
      recover "no ast in Tipes" ~debug:tl NoChange
  | TLDB _ ->
      recover "no ast in DBs" ~debug:tl NoChange
  | TLGroup _ ->
      recover "no ast in Groups" ~debug:tl NoChange


let replace (p : blankOrData) (replacement : blankOrData) (tl : toplevel) :
    toplevel =
  let id = P.toID p in
  match replacement with
  | PEventName bo | PEventModifier bo | PEventSpace bo ->
    ( match asHandler tl with
    | Some h ->
        let newSpec = SpecHeaders.replace id bo h.spec in
        TLHandler {h with spec = newSpec}
    | _ ->
        recover "Changing handler metadata on non-handler" ~debug:replacement tl
    )
  | PDBName _ | PDBColType _ | PDBColName _ ->
      tl
  | PFnName _ | PFnReturnTipe _ | PParamName _ | PParamTipe _ ->
    ( match asUserFunction tl with
    | Some fn ->
        let newFn = UserFunctions.replaceMetadataField p replacement fn in
        TLFunc newFn
    | _ ->
        recover "Changing fn metadata on non-fn" ~debug:replacement tl )
  | PTypeName _ | PTypeFieldName _ | PTypeFieldTipe _ ->
    ( match asUserTipe tl with
    | Some tipe ->
        let newTL = UserTypes.replace p replacement tipe in
        TLTipe newTL
    | _ ->
        recover "Changing tipe metadata on non-tipe" ~debug:replacement tl )
  | PGroupName _ ->
    ( match asGroup tl with
    | Some group ->
        let newTL = Groups.replace p replacement group in
        TLGroup newTL
    | _ ->
        recover "Changing group metadata on non-fn" ~debug:replacement tl )


let combine
    (handlers : handler TD.t)
    (dbs : db TD.t)
    (userFunctions : userFunction TD.t)
    (packageFn : packageFn TD.t)
    (userTipes : userTipe TD.t)
    (groups : group TD.t) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) dbs)
  |> TD.mergeLeft (TD.map ~f:ufToTL userFunctions)
  |> TD.mergeLeft (TD.map ~f:pmfToTL packageFn)
  |> TD.mergeLeft (TD.map ~f:utToTL userTipes)
  |> TD.mergeLeft (TD.map ~f:(fun group -> TLGroup group) groups)


let all (m : model) : toplevel TD.t =
  combine
    m.handlers
    m.dbs
    m.userFunctions
    m.functions.packageFunctions
    m.userTipes
    m.groups


let structural (m : model) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) m.handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) m.dbs)
  |> TD.mergeLeft (TD.map ~f:(fun group -> TLGroup group) m.groups)


let get (m : model) (tlid : TLID.t) : toplevel option = TD.get ~tlid (all m)

let find (tl : toplevel) (id_ : ID.t) : blankOrData option =
  blankOrData tl
  |> List.filter ~f:(fun d -> id_ = P.toID d)
  |> assertFn
       "cant find pd for id"
       ~debug:(id tl, id)
       ~f:(fun r -> List.length r <= 1)
  (* guard against dups *)
  |> List.head


let getPD (m : model) (tlid : TLID.t) (id : ID.t) : blankOrData option =
  get m tlid |> Option.andThen ~f:(fun tl -> find tl id)


let getTLAndPD (m : model) (tlid : TLID.t) (id : ID.t) :
    (toplevel * blankOrData option) option =
  get m tlid |> Option.map ~f:(fun tl -> (tl, find tl id))


let allDBNames (dbs : db TD.t) : string list =
  dbs
  |> TD.filterMapValues ~f:(fun db ->
         match db.dbName with F (_, name) -> Some name | Blank _ -> None)


let allGloballyScopedVarnames (dbs : db TD.t) : string list = allDBNames dbs

let asPage (tl : toplevel) (center : bool) : page =
  match tl with
  | TLHandler _ ->
      FocusedHandler (id tl, None, center)
  | TLDB _ ->
      FocusedDB (id tl, center)
  | TLGroup _ ->
      FocusedGroup (id tl, center)
  | TLPmFunc _ | TLFunc _ ->
      FocusedFn (id tl, None)
  | TLTipe _ ->
      FocusedType (id tl)


let selected (m : model) : toplevel option =
  m.cursorState |> CursorState.tlidOf |> Option.andThen ~f:(get m)


let selectedAST (m : model) : FluidAST.t option =
  selected m |> Option.andThen ~f:getAST


(* Sends updated AST to ops, modifies Toplevel's AST in model.
* To ensure we have synced up changes we should always try to use this function instead of calling setASTOpMod or updateModelWithAST individually.
*)
let updateAST
    ?(mFn : model -> model = fun m -> m) (tl : toplevel) (ast : FluidAST.t) :
    modification =
  (* Let's keep ops-related mods as is, because the code handling modification AddOps (handleAPI) is rather complicated.
    For now we want to focus on deprecating client-model updating modifications
  *)
  let opsMod = setASTOpMod tl ast in
  Many
    [ opsMod
    ; ReplaceAllModificationsWithThisOne
        (fun m ->
          let tlid = id tl in
          (* All model updates happens here *)
          let newM =
            (* Apply model update function passed by caller *)
            mFn m
            (* Updates model AST directly instead of waiting for API callback *)
            |> fun m' -> updateModelWithAST m' tlid ast
          in
          (newM, Tea.Cmd.none)) ]


(* ------------------------- *)
(* Blanks *)
(* ------------------------- *)

type predecessor = ID.t option

type successor = ID.t option

let allBlanks (tl : toplevel) : ID.t list =
  (tl |> blankOrData |> List.filter ~f:P.isBlank |> List.map ~f:P.toID)
  @ ( tl
    |> getAST
    |> Option.map ~f:FluidAST.blanks
    |> Option.withDefault ~default:[]
    |> List.map ~f:FluidExpression.toID )


let allIDs (tl : toplevel) : ID.t list =
  (tl |> blankOrData |> List.map ~f:P.toID)
  @ ( tl
    |> getAST
    |> Option.map ~f:FluidAST.ids
    |> Option.withDefault ~default:[] )


let firstBlank (tl : toplevel) : successor = tl |> allBlanks |> List.head

let lastBlank (tl : toplevel) : successor = tl |> allBlanks |> List.last

let getNextBlank (tl : toplevel) (id : ID.t) : successor =
  let all = allIDs tl in
  let index =
    List.elemIndex ~value:id all |> Option.withDefault ~default:(-1)
  in
  let blanks = allBlanks tl |> List.map ~f:ID.toString |> StrSet.fromList in
  all
  |> List.drop ~count:(index + 1)
  |> List.find ~f:(fun id -> StrSet.has blanks ~value:(ID.toString id))
  |> Option.orElse (firstBlank tl)


let getPrevBlank (tl : toplevel) (id : ID.t) : predecessor =
  let all = allIDs tl in
  let index =
    List.elemIndex ~value:id all
    |> Option.withDefault ~default:(List.length all)
  in
  let blanks = allBlanks tl |> List.map ~f:ID.toString |> StrSet.fromList in
  all
  |> List.take ~count:index
  |> List.reverse
  |> List.find ~f:(fun id -> StrSet.has blanks ~value:(ID.toString id))
  |> Option.orElse (lastBlank tl)
