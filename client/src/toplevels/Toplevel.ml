open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
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
  | TLFunc f ->
      recover "no pos in a func" ~debug:f.ufTLID {x = 0; y = 0}
  | TLTipe t ->
      recover "no pos in a tipe" ~debug:t.utTLID {x = 0; y = 0}


let remove (m : model) (tl : toplevel) : model =
  match tl with
  | TLHandler h ->
      Handlers.remove m h
  | TLDB db ->
      DB.remove m db
  | TLFunc f ->
      Functions.remove m f
  | TLTipe ut ->
      UserTypes.remove m ut
  | TLGroup g ->
      Groups.remove m g


let fromList (tls : toplevel list) : toplevel TLIDDict.t =
  tls |> List.map ~f:(fun tl -> (id tl, tl)) |> TD.fromList


let move (tlid : tlid) (xOffset : int) (yOffset : int) (m : model) : model =
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

let utToTL (ut : userTipe) : toplevel = TLTipe ut

let asUserFunction (tl : toplevel) : userFunction option =
  match tl with TLFunc f -> Some f | _ -> None


let asUserTipe (tl : toplevel) : userTipe option =
  match tl with TLTipe t -> Some t | _ -> None


let asGroup (tl : toplevel) : group option =
  match tl with TLGroup g -> Some g | _ -> None


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
  | TLFunc f ->
      Functions.blankOrData f
  | TLTipe t ->
      UserTypes.blankOrData t
  | TLGroup g ->
      Groups.blankOrData g


let isValidBlankOrID (tl : toplevel) (id : id) : bool =
  List.member ~value:id (tl |> blankOrData |> List.map ~f:P.toID)


(* ------------------------- *)
(* ASTs *)
(* ------------------------- *)

let getAST (tl : toplevel) : fluidExpr option =
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | _ ->
      None


let setAST (tl : toplevel) (newAST : fluidExpr) : toplevel =
  match tl with
  | TLHandler h ->
      TLHandler {h with ast = newAST}
  | TLFunc uf ->
      TLFunc {uf with ufAST = newAST}
  | TLDB _ | TLTipe _ | TLGroup _ ->
      tl


let withAST (m : model) (tlid : tlid) (ast : fluidExpr) : model =
  { m with
    handlers = TD.updateIfPresent m.handlers ~tlid ~f:(fun h -> {h with ast})
  ; userFunctions =
      TD.updateIfPresent m.userFunctions ~tlid ~f:(fun uf ->
          {uf with ufAST = ast}) }


let setASTMod (tl : toplevel) (ast : fluidExpr) : modification =
  match tl with
  | TLHandler h ->
      if h.ast = ast
      then NoChange
      else AddOps ([SetHandler (id tl, h.pos, {h with ast})], FocusNoChange)
  | TLFunc f ->
      if f.ufAST = ast
      then NoChange
      else AddOps ([SetFunction {f with ufAST = ast}], FocusNoChange)
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
  | PFnName _ | PParamName _ | PParamTipe _ ->
    ( match asUserFunction tl with
    | Some fn ->
        let newFn = Functions.replaceMetadataField p replacement fn in
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


let replaceOp (pd : blankOrData) (replacement : blankOrData) (tl : toplevel) :
    op list =
  let newTL = replace pd replacement tl in
  if newTL = tl
  then []
  else
    match newTL with
    | TLHandler h ->
        [SetHandler (h.hTLID, h.pos, h)]
    | TLFunc f ->
        [SetFunction f]
    | TLTipe t ->
        [SetType t]
    | TLDB _ ->
        recover "no vars in DBs" ~debug:tl []
    | TLGroup _ ->
        recover "groups are front end only" ~debug:tl []


let replaceMod (pd : blankOrData) (replacement : blankOrData) (tl : toplevel) :
    modification =
  let ops = replaceOp pd replacement tl in
  if ops = [] then NoChange else AddOps (ops, FocusNoChange)


let combine
    (handlers : handler TD.t)
    (dbs : db TD.t)
    (userFunctions : userFunction TD.t)
    (userTipes : userTipe TD.t)
    (groups : group TD.t) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) dbs)
  |> TD.mergeLeft (TD.map ~f:ufToTL userFunctions)
  |> TD.mergeLeft (TD.map ~f:utToTL userTipes)
  |> TD.mergeLeft (TD.map ~f:(fun group -> TLGroup group) groups)


let all (m : model) : toplevel TD.t =
  combine m.handlers m.dbs m.userFunctions m.userTipes m.groups


let structural (m : model) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) m.handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) m.dbs)
  |> TD.mergeLeft (TD.map ~f:(fun group -> TLGroup group) m.groups)


let get (m : model) (tlid : tlid) : toplevel option = TD.get ~tlid (all m)

let find (tl : toplevel) (id_ : id) : blankOrData option =
  blankOrData tl
  |> List.filter ~f:(fun d -> id_ = P.toID d)
  |> assertFn
       "cant find pd for id"
       ~debug:(id tl, id)
       ~f:(fun r -> List.length r <= 1)
  (* guard against dups *)
  |> List.head


let getPD (m : model) (tlid : tlid) (id : id) : blankOrData option =
  get m tlid |> Option.andThen ~f:(fun tl -> find tl id)


let getTLAndPD (m : model) (tlid : tlid) (id : id) :
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
      FocusedHandler (id tl, center)
  | TLDB _ ->
      FocusedDB (id tl, center)
  | TLGroup _ ->
      FocusedGroup (id tl, center)
  | TLFunc _ ->
      FocusedFn (id tl)
  | TLTipe _ ->
      FocusedType (id tl)


let selected (m : model) : toplevel option =
  m.cursorState |> tlidOf |> Option.andThen ~f:(get m)


let selectedAST (m : model) : fluidExpr option =
  selected m |> Option.andThen ~f:getAST


let setSelectedAST (m : model) (ast : fluidExpr) : modification =
  match selected m with None -> NoChange | Some tl -> setASTMod tl ast


(* ------------------------- *)
(* Blanks *)
(* ------------------------- *)

type predecessor = id option

type successor = id option

let allBlanks (tl : toplevel) : id list =
  (tl |> blankOrData |> List.filter ~f:P.isBlank |> List.map ~f:P.toID)
  @ ( tl
    |> getAST
    |> Option.map ~f:AST.blanks
    |> Option.withDefault ~default:[]
    |> List.map ~f:FluidExpression.id )


let allIDs (tl : toplevel) : id list =
  (tl |> blankOrData |> List.map ~f:P.toID)
  @ (tl |> getAST |> Option.map ~f:AST.ids |> Option.withDefault ~default:[])


let firstBlank (tl : toplevel) : successor = tl |> allBlanks |> List.head

let lastBlank (tl : toplevel) : successor = tl |> allBlanks |> List.last

let getNextBlank (tl : toplevel) (id : id) : successor =
  let all = allIDs tl in
  let index =
    List.elemIndex ~value:id all |> Option.withDefault ~default:(-1)
  in
  let blanks = allBlanks tl |> List.map ~f:deID |> StrSet.fromList in
  all
  |> List.drop ~count:(index + 1)
  |> List.find ~f:(fun (ID id) -> StrSet.has blanks ~value:id)
  |> Option.orElse (firstBlank tl)


let getPrevBlank (tl : toplevel) (id : id) : predecessor =
  let all = allIDs tl in
  let index =
    List.elemIndex ~value:id all
    |> Option.withDefault ~default:(List.length all)
  in
  let blanks = allBlanks tl |> List.map ~f:deID |> StrSet.fromList in
  all
  |> List.take ~count:index
  |> List.reverse
  |> List.find ~f:(fun (ID id) -> StrSet.has blanks ~value:id)
  |> Option.orElse (lastBlank tl)
