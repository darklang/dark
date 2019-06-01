open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

type predecessor = pointerData option

type successor = pointerData option

type dbReference = tlid * dBColumn list

(* ------------------------- *)
(* Toplevel manipulation *)
(* ------------------------- *)
let name (tl : toplevel) : string =
  match tl.data with
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
  match tl.data with
  | TLHandler h ->
      (h.spec.module_ |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.name |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ (h.spec.modifier |> B.toMaybe |> Option.withDefault ~default:"")
  | TLDB db ->
      db.dbName |> B.toMaybe |> Option.withDefault ~default:"Undefined"
  | TLFunc f ->
      f.ufMetadata.ufmName
      |> B.toMaybe
      |> Option.withDefault ~default:"Unnamed"
  | TLTipe t ->
      t.utName |> B.toMaybe |> Option.withDefault ~default:"Unnamed"


let containsByTLID (tls : toplevel list) (elem : toplevel) : bool =
  List.find ~f:(fun tl -> tl.id = elem.id) tls <> None


let removeByTLID ~(toBeRemoved : toplevel list) (origTls : toplevel list) :
    toplevel list =
  List.filter
    ~f:(fun origTl -> not (containsByTLID toBeRemoved origTl))
    origTls


let upsertByTLID (tls : toplevel list) (tl : toplevel) : toplevel list =
  removeByTLID tls ~toBeRemoved:[tl] @ [tl]


let upsert (m : model) (tl : toplevel) : model =
  {m with toplevels = upsertByTLID m.toplevels tl}


let upsertAllByTLID (tls : toplevel list) ~(newTLs : toplevel list) :
    toplevel list =
  List.foldl ~f:(fun tl tls -> upsertByTLID tls tl) ~init:tls newTLs


let upsertAll (m : model) (tls : toplevel list) : model =
  List.foldl ~f:(fun tl m -> upsert m tl) ~init:m tls


let remove (m : model) (tl : toplevel) : model =
  {m with toplevels = removeByTLID m.toplevels ~toBeRemoved:[tl]}


let updateByTLID (tls : toplevel list) (tlid : tlid) (f : toplevel -> toplevel)
    : toplevel list =
  tls |> List.map ~f:(fun t -> if t.id <> tlid then t else f t)


let update (m : model) (tlid : tlid) ~(f : toplevel -> toplevel) : model =
  {m with toplevels = updateByTLID m.toplevels tlid f}


let moveTL (xOffset : int) (yOffset : int) (tl : toplevel) : toplevel =
  let newPos = {x = tl.pos.x + xOffset; y = tl.pos.y + yOffset} in
  {tl with pos = newPos}


let move (tlid : tlid) (xOffset : int) (yOffset : int) (m : model) : model =
  update m tlid ~f:(moveTL xOffset yOffset)


let ufToTL (uf : userFunction) : toplevel =
  {id = uf.ufTLID; pos = Defaults.centerPos; data = TLFunc uf}


let utToTL (ut : userTipe) : toplevel =
  {id = ut.utTLID; pos = Defaults.centerPos; data = TLTipe ut}


let asUserFunction (tl : toplevel) : userFunction option =
  match tl.data with TLFunc f -> Some f | _ -> None


let asUserTipe (tl : toplevel) : userTipe option =
  match tl.data with TLTipe t -> Some t | _ -> None


let isUserTipe (tl : toplevel) : bool =
  match tl.data with TLTipe _ -> true | _ -> false


let asHandler (tl : toplevel) : handler option =
  match tl.data with TLHandler h -> Some h | _ -> None


let asDB (tl : toplevel) : dB option =
  match tl.data with TLDB h -> Some h | _ -> None


let isDB (tl : toplevel) : bool =
  match tl.data with TLDB _ -> true | _ -> false


let isHandler (tl : toplevel) : bool =
  match tl.data with TLHandler _ -> true | _ -> false


let handlers (tls : toplevel list) : handler list =
  List.filterMap ~f:asHandler tls


let astOf (tl : toplevel) : expr option =
  match tl.data with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | _ ->
      None


let dbs (tls : toplevel list) : dB list = List.filterMap ~f:asDB tls

let spaceOfHandler (h : handler) : handlerSpace = SpecHeaders.spaceOf h.spec

let spaceOf (tl : toplevel) : handlerSpace option =
  tl |> asHandler |> Option.map ~f:spaceOfHandler


let isHTTPHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSHTTP)

let isCronHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSCron)

let isCustomEventSpaceHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSOther)


let isUndefinedEventSpaceHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSEmpty)


let toOp (tl : toplevel) : op list =
  match tl.data with
  | TLHandler h ->
      [SetHandler (tl.id, tl.pos, h)]
  | TLFunc fn ->
      [SetFunction fn]
  | TLTipe t ->
      [SetType t]
  | TLDB _ ->
      impossible "This isn't how database ops work"


let customEventSpaceNames (toplevels : toplevel list) : string list =
  let otherSpaces =
    toplevels
    |> List.filter ~f:isCustomEventSpaceHandler
    |> List.filterMap ~f:(fun tl ->
           asHandler tl |> Option.andThen ~f:(fun h -> B.toMaybe h.spec.module_)
       )
  in
  otherSpaces


(* ------------------------- *)
(* Generic *)
(* ------------------------- *)
let allData (tl : toplevel) : pointerData list =
  match tl.data with
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
  match tl.data with
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
    match tl.data with
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


(* TODO(match) *)

let firstChild (tl : toplevel) (id : pointerData) : pointerData option =
  getChildrenOf tl id |> List.head


let rootExpr (tl : toplevel) : expr option =
  (* TODO SpecTypePointerDataRefactor *)
  match tl.data with
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
  let ha () = tl |> asHandler |> deOption "TL.replace ha ()" in
  let fn () = tl |> asUserFunction |> deOption "TL.replace fn ()" in
  let tipe () = tl |> asUserTipe |> deOption "TL.replace tipe ()" in
  let id = P.toID p in
  let astReplace () =
    match tl.data with
    | TLHandler h ->
        let newAST = AST.replace p replacement h.ast in
        {tl with data = TLHandler {h with ast = newAST}}
    | TLFunc f ->
        let newAST = AST.replace p replacement f.ufAST in
        {tl with data = TLFunc {f with ufAST = newAST}}
    | TLDB _ | TLTipe _ ->
        impossible ("no AST here", tl.data)
  in
  let specHeaderReplace bo =
    let h = ha () in
    let newSpec = SpecHeaders.replace id bo h.spec in
    {tl with data = TLHandler {h with spec = newSpec}}
  in
  let fnMetadataReplace () =
    let f = fn () in
    let newF = Functions.replaceMetadataField p replacement f in
    {tl with data = TLFunc newF}
  in
  let tipeReplace () =
    let t = tipe () in
    let newTipe = UserTypes.replace p replacement t in
    {tl with data = TLTipe newTipe}
  in
  match replacement with
  | PVarBind _ ->
      astReplace ()
  | PField _ ->
      astReplace ()
  | PKey _ ->
      astReplace ()
  | PExpr _ ->
      astReplace ()
  | PEventName en ->
      specHeaderReplace en
  | PEventModifier em ->
      specHeaderReplace em
  | PEventSpace es ->
      specHeaderReplace es
  | PDBName _ ->
      tl
  | PDBColType _ ->
      tl
  (* SetDBColType tl.id id (tipe |> B.toMaybe |> deMaybe "replace - tipe") *)
  | PDBColName _ ->
      tl
  (* SetDBColName tl.id id (name |> B.toMaybe |> deMaybe "replace - name") *)
  | PFFMsg bo ->
    ( match tl.data with
    | TLHandler h ->
        let spec2 = SpecHeaders.replace id bo h.spec in
        let ast = AST.replace p replacement h.ast in
        {tl with data = TLHandler {h with spec = spec2; ast}}
    | TLFunc f ->
        let ast = AST.replace p replacement f.ufAST in
        {tl with data = TLFunc {f with ufAST = ast}}
    | TLDB _ | TLTipe _ ->
        tl )
  | PFnName _ ->
      fnMetadataReplace ()
  | PParamName _ ->
      fnMetadataReplace ()
  | PParamTipe _ ->
      fnMetadataReplace ()
  | PPattern _ ->
      astReplace ()
  | PConstructorName _ ->
      astReplace ()
  | PFnCallName _ ->
      tl
  | PTypeName _ ->
      tipeReplace ()
  | PTypeFieldName _ ->
      tipeReplace ()
  | PTypeFieldTipe _ ->
      tipeReplace ()


let replaceMod (pd : pointerData) (replacement : pointerData) (tl : toplevel) :
    modification =
  let newTL = replace pd replacement tl in
  if newTL = tl
  then NoChange
  else
    match newTL.data with
    | TLHandler h ->
        let ops = [SetHandler (tl.id, tl.pos, h)] in
        RPC (ops, FocusNoChange)
    | TLFunc f ->
        let ops = [SetFunction f] in
        RPC (ops, FocusNoChange)
    | TLTipe t ->
        let ops = [SetType t] in
        RPC (ops, FocusNoChange)
    | TLDB _ ->
        impossible ("no vars in DBs", tl.data)


(* do nothing for now *)

let delete (tl : toplevel) (p : pointerData) (newID : id) : toplevel =
  let replacement = P.emptyD_ newID (P.typeOf p) in
  replace p replacement tl


let all (m : model) : toplevel list =
  m.toplevels
  @ List.map ~f:ufToTL m.userFunctions
  @ List.map ~f:utToTL m.userTipes


let get (m : model) (id : tlid) : toplevel option =
  let tls = all m in
  List.find ~f:(fun tl -> tl.id = id) tls


let getTL (m : model) (id : tlid) : toplevel = get m id |> deOption "getTL"

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


let allDBNames (toplevels : toplevel list) : string list =
  toplevels
  |> List.filterMap ~f:(fun tl ->
         match tl.data with
         | TLDB db ->
           ( match db.dbName with
           | F (_, name) ->
               Some name
           | Partial _ | Blank _ ->
               None )
         | _ ->
             None )


let allGloballyScopedVarnames (toplevels : toplevel list) : string list =
  allDBNames toplevels


let asPage (tl : toplevel) (center : bool) : page =
  match tl.data with
  | TLHandler _ ->
      FocusedHandler (tl.id, center)
  | TLDB _ ->
      FocusedDB (tl.id, center)
  | TLFunc _ ->
      FocusedFn tl.id
  | TLTipe _ ->
      FocusedType tl.id


let selected (m : model) : toplevel option =
  m.cursorState |> tlidOf |> Option.andThen ~f:(get m)


let selectedAST (m : model) : expr option =
  selected m |> Option.andThen ~f:rootExpr


let setSelectedAST (m : model) (ast : expr) : modification =
  match selected m with
  | None ->
      NoChange
  | Some tl ->
    ( match tl.data with
    | TLHandler h ->
        RPC ([SetHandler (tl.id, tl.pos, {h with ast})], FocusNoChange)
    | TLFunc f ->
        RPC ([SetFunction {f with ufAST = ast}], FocusNoChange)
    | TLTipe _ ->
        impossible ("no ast in Tipes", tl.data)
    | TLDB _ ->
        impossible ("no ast in DBs", tl.data) )


let withAST (m : model) (tlid : tlid) (ast : expr) : model =
  update m tlid ~f:(fun tl ->
      let data =
        match tl.data with
        | TLHandler h ->
            TLHandler {h with ast}
        | TLFunc f ->
            TLFunc {f with ufAST = ast}
        | TLTipe _ ->
            impossible ("no ast in Tipes", tl.data)
        | TLDB _ ->
            impossible ("no ast in DBs", tl.data)
      in
      {tl with data} )
