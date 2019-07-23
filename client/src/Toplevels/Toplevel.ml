open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

type predecessor = pointerData option

type successor = pointerData option

type dbReference = tlid * dbColumn list

(* ------------------------- *)
(* Toplevel manipulation *)
(* ------------------------- *)
let name (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      "H: " ^ (h.spec.name |> B.toMaybe |> Option.withDefault ~default:"")
  | TLDB db ->
      "DB: " ^ (db.dbName |> B.toMaybe |> Option.withDefault ~default:"")
  | TLFunc f ->
      "Func: "
      ^ (f.ufMetadata.ufmName |> B.toMaybe |> Option.withDefault ~default:"")
  | TLTipe t ->
      "Type: " ^ (t.utName |> B.toMaybe |> Option.withDefault ~default:"")


let sortkey (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      (h.spec.space |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.name |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.modifier |> B.toMaybe |> Option.withDefault ~default:"")
  | TLDB db ->
      db.dbName |> B.toMaybe |> Option.withDefault ~default:"Undefined"
  | TLFunc f ->
      f.ufMetadata.ufmName |> B.toMaybe |> Option.withDefault ~default:""
  | TLTipe t ->
      t.utName |> B.toMaybe |> Option.withDefault ~default:""


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


let pos tl =
  match tl with
  | TLHandler h ->
      h.pos
  | TLDB db ->
      db.pos
  | TLFunc _ ->
      Debug.crash "no pos in a func"
  | TLTipe _ ->
      Debug.crash "no pos in a tipe"


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


let fromList (tls : toplevel list) : toplevel TLIDDict.t =
  tls |> List.map ~f:(fun tl -> (id tl, tl)) |> TD.fromList


let move (tlid : tlid) (xOffset : int) (yOffset : int) (m : model) : model =
  let newPos p = {x = p.x + xOffset; y = p.y + yOffset} in
  { m with
    handlers =
      TD.updateIfPresent m.handlers ~tlid ~f:(fun (h : handler) ->
          {h with pos = newPos h.pos} )
  ; dbs =
      TD.updateIfPresent m.dbs ~tlid ~f:(fun (db : db) ->
          {db with pos = newPos db.pos} ) }


let ufToTL (uf : userFunction) : toplevel = TLFunc uf

let utToTL (ut : userTipe) : toplevel = TLTipe ut

let asUserFunction (tl : toplevel) : userFunction option =
  match tl with TLFunc f -> Some f | _ -> None


let asUserTipe (tl : toplevel) : userTipe option =
  match tl with TLTipe t -> Some t | _ -> None


let isUserTipe (tl : toplevel) : bool =
  match tl with TLTipe _ -> true | _ -> false


let asHandler (tl : toplevel) : handler option =
  match tl with TLHandler h -> Some h | _ -> None


let asDB (tl : toplevel) : db option =
  match tl with TLDB h -> Some h | _ -> None


let isDB (tl : toplevel) : bool = match tl with TLDB _ -> true | _ -> false

let isHandler (tl : toplevel) : bool =
  match tl with TLHandler _ -> true | _ -> false


let handlers (tls : toplevel list) : handler list =
  List.filterMap ~f:asHandler tls


let astOf (tl : toplevel) : expr option =
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | TLTipe _ | TLDB _ ->
      None


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
  | TLDB _ ->
      impossible "This isn't how database ops work"


let customEventSpaceNames (handlers : handler TD.t) : string list =
  let otherSpaces =
    handlers
    |> TD.mapValues ~f:(fun h -> TLHandler h)
    |> List.filter ~f:isDeprecatedCustomHandler
    |> List.filterMap ~f:(fun tl ->
           asHandler tl |> Option.andThen ~f:(fun h -> B.toMaybe h.spec.space)
       )
  in
  otherSpaces


(* ------------------------- *)
(* Generic *)
(* ------------------------- *)
let allData (tl : toplevel) : pointerData list =
  match tl with
  | TLHandler h ->
      SpecHeaders.allData h.spec @ AST.allData h.ast
  | TLDB db ->
      DB.allData db
  | TLFunc f ->
      Functions.allData f
  | TLTipe t ->
      UserTypes.allData t


let isValidID (tl : toplevel) (id : id) : bool =
  List.member ~value:id (tl |> allData |> List.map ~f:P.toID)


let clonePointerData (pd : pointerData) : pointerData =
  match pd with
  | PVarBind vb ->
      PVarBind (B.clone identity vb)
  | PEventModifier sp ->
      PEventModifier (B.clone identity sp)
  | PEventName sp ->
      PEventName (B.clone identity sp)
  | PEventSpace sp ->
      PEventSpace (B.clone identity sp)
  | PExpr expr ->
      PExpr (AST.clone expr)
  | PField f ->
      PField (B.clone identity f)
  | PKey k ->
      PKey (B.clone identity k)
  | PFFMsg msg ->
      PFFMsg (B.clone identity msg)
  | PFnName name ->
      PFnName (B.clone identity name)
  | PFnCallName name ->
      PFnCallName (B.clone identity name)
  | PParamName name ->
      PParamName (B.clone identity name)
  | PParamTipe tipe ->
      PParamTipe (B.clone identity tipe)
  | PPattern pattern ->
      PPattern (AST.clonePattern pattern)
  | PConstructorName name ->
      PConstructorName (B.clone identity name)
  | PTypeName name ->
      PTypeName (B.clone identity name)
  | PTypeFieldName name ->
      PTypeFieldName (B.clone identity name)
  | PTypeFieldTipe tipe ->
      PTypeFieldTipe (B.clone identity tipe)
  | PDBColName _ | PDBColType _ | PDBName _ ->
      pd


(* ------------------------- *)
(* Blanks *)
(* ------------------------- *)
let allBlanks (tl : toplevel) : pointerData list =
  tl |> allData |> List.filter ~f:P.isBlank


let firstBlank (tl : toplevel) : successor = tl |> allBlanks |> List.head

let lastBlank (tl : toplevel) : successor = tl |> allBlanks |> List.last

let getNextBlank (tl : toplevel) (pred : predecessor) : successor =
  match pred with
  | Some pred_ ->
      let ps = allData tl in
      let index =
        List.elemIndex ~value:pred_ ps |> Option.withDefault ~default:(-1)
      in
      let remaining = List.drop ~count:(index + 1) ps in
      let blanks = List.filter ~f:P.isBlank remaining in
      blanks |> List.head |> Option.orElse (firstBlank tl)
  | None ->
      firstBlank tl


let getPrevBlank (tl : toplevel) (next : successor) : predecessor =
  match next with
  | Some next_ ->
      let ps = allData tl in
      let index =
        List.elemIndex ~value:next_ ps
        |> Option.withDefault ~default:(List.length ps)
      in
      let remaining = List.take ~count:index ps in
      let blanks = List.filter ~f:P.isBlank remaining in
      blanks |> List.last |> Option.orElse (lastBlank tl)
  | None ->
      lastBlank tl


(* ------------------------- *)
(* Up/Down the tree *)
(* ------------------------- *)
let getParentOf (tl : toplevel) (p : pointerData) : pointerData option =
  (* TODO SpecTypePointerDataRefactor *)
  match tl with
  | TLHandler h ->
      AST.findParentOfWithin_ (P.toID p) h.ast
      |> Option.map ~f:(fun x -> PExpr x)
  | TLFunc f ->
      AST.findParentOfWithin_ (P.toID p) f.ufAST
      |> Option.map ~f:(fun x -> PExpr x)
  | TLDB db ->
      db
      |> DB.astsFor
      |> List.map ~f:(AST.findParentOfWithin_ (P.toID p))
      |> Option.values
      |> List.head
      |> Option.map ~f:(fun x -> PExpr x)
  | TLTipe _ ->
      (* Type definitions are flat *)
      None


let getChildrenOf (tl : toplevel) (pd : pointerData) : pointerData list =
  let pid = P.toID pd in
  let astChildren () =
    match tl with
    | TLHandler h ->
        AST.childrenOf pid h.ast
    | TLFunc f ->
        AST.childrenOf pid f.ufAST
    | TLDB db ->
        db |> DB.astsFor |> List.map ~f:(AST.childrenOf pid) |> List.concat
    | TLTipe _ ->
        []
  in
  match pd with
  | PVarBind _ ->
      []
  | PField _ ->
      []
  | PKey _ ->
      []
  | PExpr _ ->
      astChildren ()
  | PEventModifier _ ->
      []
  | PEventName _ ->
      []
  | PEventSpace _ ->
      []
  | PDBName _ ->
      []
  | PDBColName _ ->
      []
  | PDBColType _ ->
      []
  | PFFMsg _ ->
      []
  | PFnName _ ->
      []
  | PFnCallName _ ->
      []
  | PParamName _ ->
      []
  | PParamTipe _ ->
      []
  | PPattern _ ->
      []
  | PConstructorName _ ->
      []
  | PTypeName _ ->
      []
  | PTypeFieldName _ ->
      []
  | PTypeFieldTipe _ ->
      []


let getAST (tl : toplevel) : expr option =
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | _ ->
      None


let setAST (tl : toplevel) (newAST : expr) : toplevel =
  match tl with
  | TLHandler h ->
      TLHandler {h with ast = newAST}
  | TLFunc uf ->
      TLFunc {uf with ufAST = newAST}
  | TLDB _ | TLTipe _ ->
      tl


let withAST (m : model) (tlid : tlid) (ast : expr) : model =
  { m with
    handlers = TD.updateIfPresent m.handlers ~tlid ~f:(fun h -> {h with ast})
  ; userFunctions =
      TD.updateIfPresent m.userFunctions ~tlid ~f:(fun uf ->
          {uf with ufAST = ast} ) }


(* TODO(match) *)

let firstChild (tl : toplevel) (id : pointerData) : pointerData option =
  getChildrenOf tl id |> List.head


let rootExpr (tl : toplevel) : expr option =
  (* TODO SpecTypePointerDataRefactor *)
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | TLDB _ | TLTipe _ ->
      None


let rootOf (tl : toplevel) : pointerData option =
  (* TODO SpecTypePointerDataRefactor *)
  rootExpr tl |> Option.map ~f:(fun expr -> PExpr expr)


let replace (p : pointerData) (replacement : pointerData) (tl : toplevel) :
    toplevel =
  let id = P.toID p in
  match replacement with
  | PFFMsg _
  | PVarBind _
  | PField _
  | PKey _
  | PExpr _
  | PPattern _
  | PConstructorName _ ->
      tl
      |> getAST
      |> Option.map ~f:(fun ast -> AST.replace p replacement ast)
      |> Option.map ~f:(setAST tl)
      |> deOption "replacing an expr in a non-ast tl"
  | PEventName bo | PEventModifier bo | PEventSpace bo ->
      let h = tl |> asHandler |> deOption "TL.replace ha ()" in
      let newSpec = SpecHeaders.replace id bo h.spec in
      TLHandler {h with spec = newSpec}
  | PDBName _ | PDBColType _ | PDBColName _ | PFnCallName _ ->
      tl
  | PFnName _ | PParamName _ | PParamTipe _ ->
      let fn = tl |> asUserFunction |> deOption "TL.replace fn ()" in
      let newFn = Functions.replaceMetadataField p replacement fn in
      TLFunc newFn
  | PTypeName _ | PTypeFieldName _ | PTypeFieldTipe _ ->
      let tipe = tl |> asUserTipe |> deOption "TL.replace tipe ()" in
      let newTL = UserTypes.replace p replacement tipe in
      TLTipe newTL


let replaceOp (pd : pointerData) (replacement : pointerData) (tl : toplevel) :
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
        impossible ("no vars in DBs", tl)


let replaceMod (pd : pointerData) (replacement : pointerData) (tl : toplevel) :
    modification =
  let ops = replaceOp pd replacement tl in
  if ops = [] then NoChange else RPC (ops, FocusNoChange)


(* do nothing for now *)

let delete (tl : toplevel) (p : pointerData) (newID : id) : toplevel =
  let replacement = P.emptyD_ newID (P.typeOf p) in
  replace p replacement tl


let combine
    (handlers : handler TD.t)
    (dbs : db TD.t)
    (userFunctions : userFunction TD.t)
    (userTipes : userTipe TD.t) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) dbs)
  |> TD.mergeLeft (TD.map ~f:ufToTL userFunctions)
  |> TD.mergeLeft (TD.map ~f:utToTL userTipes)


let all (m : model) : toplevel TD.t =
  combine m.handlers m.dbs m.userFunctions m.userTipes


let structural (m : model) : toplevel TD.t =
  TD.map ~f:(fun h -> TLHandler h) m.handlers
  |> TD.mergeLeft (TD.map ~f:(fun db -> TLDB db) m.dbs)


let get (m : model) (tlid : tlid) : toplevel option = TD.get ~tlid (all m)

let getExn (m : model) (id : tlid) : toplevel =
  get m id |> deOption "TL.getExn"


let find (tl : toplevel) (id : id) : pointerData option =
  allData tl
  |> List.filter ~f:(fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  (* guard against dups *)
  |> List.head


let findExn (tl : toplevel) (id : id) : pointerData =
  find tl id |> deOption "findExn"


let getCurrent (m : model) : (toplevel * pointerData) option =
  match (tlidOf m.cursorState, idOf m.cursorState) with
  | Some tlid, Some id ->
      get m tlid
      |> Option.andThen ~f:(fun tl ->
             Option.map (find tl id) ~f:(fun pd -> (tl, pd)) )
  | _ ->
      None


let allDBNames (dbs : db TD.t) : string list =
  dbs
  |> TD.filterMapValues ~f:(fun db ->
         match db.dbName with F (_, name) -> Some name | Blank _ -> None )


let allGloballyScopedVarnames (dbs : db TD.t) : string list = allDBNames dbs

let asPage (tl : toplevel) (center : bool) : page =
  match tl with
  | TLHandler _ ->
      FocusedHandler (id tl, center)
  | TLDB _ ->
      FocusedDB (id tl, center)
  | TLFunc _ ->
      FocusedFn (id tl)
  | TLTipe _ ->
      FocusedType (id tl)


let selected (m : model) : toplevel option =
  m.cursorState |> tlidOf |> Option.andThen ~f:(get m)


let selectedAST (m : model) : expr option =
  selected m |> Option.andThen ~f:rootExpr


let setSelectedAST (m : model) (ast : expr) : modification =
  match selected m with
  | None ->
      NoChange
  | Some tl ->
    ( match tl with
    | TLHandler h ->
        RPC ([SetHandler (id tl, h.pos, {h with ast})], FocusNoChange)
    | TLFunc f ->
        RPC ([SetFunction {f with ufAST = ast}], FocusNoChange)
    | TLTipe _ ->
        impossible ("no ast in Tipes", tl)
    | TLDB _ ->
        impossible ("no ast in DBs", tl) )
