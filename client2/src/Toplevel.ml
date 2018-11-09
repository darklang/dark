open Tea
open! Porting
module B = Blank
module Fns = Functions
module P = Pointer
open Prelude
open Types

let name (tl : toplevel) : string =
  match tl.data with
  | TLHandler h -> "H: " ^ (h.spec.name |> B.toMaybe |> Option.withDefault "")
  | TLDB db -> "DB: " ^ db.dbName
  | TLFunc f ->
      "Func: " ^ (f.ufMetadata.ufmName |> B.toMaybe |> Option.withDefault "")

let containsByTLID (tls : toplevel list) (elem : toplevel) : bool =
  List.find (fun tl -> tl.id = elem.id) tls <> None

let removeByTLID (origTls : toplevel list) (toBeRemoved : toplevel list) :
    toplevel list =
  List.filter (fun origTl -> not (containsByTLID toBeRemoved origTl)) origTls

let upsertByTLID (tls : toplevel list) (tl : toplevel) : toplevel list =
  removeByTLID tls [tl] @ [tl]

let upsert (m : model) (tl : toplevel) : model =
  {m with toplevels= upsertByTLID m.toplevels tl}

let upsertAllByTLID (tls : toplevel list) (news : toplevel list) :
    toplevel list =
  List.foldl (fun tl new_ -> upsertByTLID new_ tl) tls news

let upsertAll (m : model) (tls : toplevel list) : model =
  List.foldl (fun tl m -> upsert m tl) m tls

let remove (m : model) (tl : toplevel) : model =
  {m with toplevels= removeByTLID m.toplevels [tl]}

let updateByTLID (tls : toplevel list) (tlid : tlid) (f : toplevel -> toplevel)
    : toplevel list =
  tls |> List.map (fun t -> if t.id <> tlid then t else f t)

let update (m : model) (tlid : tlid) (f : toplevel -> toplevel) : model =
  {m with toplevels= updateByTLID m.toplevels tlid f}

let moveTL (xOffset : int) (yOffset : int) (tl : toplevel) : toplevel =
  let newPos = {x= tl.pos.x + xOffset; y= tl.pos.y + yOffset} in
  {tl with pos= newPos}

let move (tlid : tlid) (xOffset : int) (yOffset : int) (m : model) : model =
  update m tlid (moveTL xOffset yOffset)

let ufToTL (m : model) (uf : userFunction) : toplevel =
  {id= uf.ufTLID; pos= Defaults.centerPos; data= TLFunc uf}

let asUserFunction (tl : toplevel) : userFunction option =
  match tl.data with TLFunc f -> Some f | _ -> None

let asHandler (tl : toplevel) : handler option =
  match tl.data with TLHandler h -> Some h | _ -> None

let asDB (tl : toplevel) : dB option =
  match tl.data with TLDB h -> Some h | _ -> None

let handlers (tls : toplevel list) : handler list =
  List.filterMap asHandler tls

let dbs (tls : toplevel list) : dB list = List.filterMap asDB tls

let spaceOfHandler (h : handler) : handlerSpace = SpecHeaders.spaceOf h.spec

let spaceOf (tl : toplevel) : handlerSpace option =
  tl |> asHandler |> Option.map spaceOfHandler

let isHTTPHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSHTTP)

let toOp (tl : toplevel) : op list =
  match tl.data with
  | TLHandler h -> [SetHandler (tl.id, tl.pos, h)]
  | TLFunc fn -> [SetFunction fn]
  | _ -> impossible "This isn't how database ops work"

let rec allData (tl : toplevel) : pointerData list =
  match tl.data with
  | TLHandler h -> SpecHeaders.allData h.spec @ AST.allData h.ast
  | TLDB db -> DB.allData db
  | TLFunc f -> Fns.allData f

let isValidID (tl : toplevel) (id : id) : bool =
  List.member id (tl |> allData |> List.map P.toID)

let clonePointerData (pd : pointerData) : pointerData =
  match pd with
  | PVarBind vb -> PVarBind (B.clone identity vb)
  | PEventModifier sp -> PEventModifier (B.clone identity sp)
  | PEventName sp -> PEventName (B.clone identity sp)
  | PEventSpace sp -> PEventSpace (B.clone identity sp)
  | PExpr expr -> PExpr (AST.clone expr)
  | PField f -> PField (B.clone identity f)
  | PKey k -> PKey (B.clone identity k)
  | PDBColName cn -> pd
  | PDBColType ct -> pd
  | PFFMsg msg -> PFFMsg (B.clone identity msg)
  | PFnName name_ -> PFnName (B.clone identity name_)
  | PParamName name_ -> PParamName (B.clone identity name_)
  | PParamTipe tipe -> PParamTipe (B.clone identity tipe)
  | PPattern pattern -> PPattern (AST.clonePattern pattern)

let allBlanks (tl : toplevel) : pointerData list =
  tl |> allData |> List.filter P.isBlank

let blanksWhere (fn : pointerData -> bool) (tl : toplevel) : pointerData list =
  tl |> allBlanks |> List.filter fn

let firstBlank (tl : toplevel) : successor = tl |> allBlanks |> List.head

let lastBlank (tl : toplevel) : successor = tl |> allBlanks |> List.last

let getNextBlank (tl : toplevel) (pred : predecessor) : successor =
  match pred with
  | Some pred_ ->
      let ps = allData tl in
      let index = List.elemIndex pred_ ps |> Option.withDefault (-1) in
      let remaining = List.drop (index + 1) ps in
      let blanks = List.filter P.isBlank remaining in
      blanks |> List.head |> Option.orElse (firstBlank tl)
  | None -> firstBlank tl

let getPrevBlank (tl : toplevel) (next : successor) : predecessor =
  match next with
  | Some next_ ->
      let ps = allData tl in
      let index =
        List.elemIndex next_ ps |> Option.withDefault (List.length ps)
      in
      let remaining = List.take index ps in
      let blanks = List.filter P.isBlank remaining in
      blanks |> List.last |> Option.orElse (lastBlank tl)
  | None -> lastBlank tl

let siblings (tl : toplevel) (p : pointerData) : pointerData list =
  match tl.data with
  | TLHandler h ->
      let toplevels = SpecHeaders.allData h.spec @ [PExpr h.ast] in
      if List.member p toplevels then toplevels else AST.siblings p h.ast
  | TLDB db -> DB.siblings p db
  | TLFunc f -> AST.siblings p f.ufAST

let getNextSibling (tl : toplevel) (p : pointerData) : pointerData =
  siblings tl p |> Util.listNextWrap p |> deOption "nextSibling"

let getPrevSibling (tl : toplevel) (p : pointerData) : pointerData =
  siblings tl p |> Util.listPreviousWrap p |> deOption "prevSibling"

let getParentOf (tl : toplevel) (p : pointerData) : pointerData option =
  match tl.data with
  | TLHandler h ->
      AST.parentOf_ (P.toID p) h.ast |> Option.map (fun x -> PExpr x)
  | TLFunc f ->
      AST.parentOf_ (P.toID p) f.ufAST |> Option.map (fun x -> PExpr x)
  | TLDB db ->
      db |> DB.astsFor
      |> List.map (AST.parentOf_ (P.toID p))
      |> Option.values |> List.head
      |> Option.map (fun x -> PExpr x)

let getChildrenOf (tl : toplevel) (pd : pointerData) : pointerData list =
  let pid = P.toID pd in
  let astChildren () =
    match tl.data with
    | TLHandler h -> AST.childrenOf pid h.ast
    | TLFunc f -> AST.childrenOf pid f.ufAST
    | TLDB db ->
        db |> DB.astsFor |> List.map (AST.childrenOf pid) |> List.concat
  in
  match pd with
  | PVarBind _ -> []
  | PField d -> []
  | PKey d -> []
  | PExpr _ -> astChildren ()
  | PEventModifier d -> []
  | PEventName d -> []
  | PEventSpace d -> []
  | PDBColName d -> []
  | PDBColType d -> []
  | PFFMsg _ -> []
  | PFnName _ -> []
  | PParamName _ -> []
  | PParamTipe _ -> []
  | PPattern _ -> [] (* TODO(match) *)

let firstChild (tl : toplevel) (id : pointerData) : pointerData option =
  getChildrenOf tl id |> List.head

let rootOf (tl : toplevel) : pointerData option =
  match tl.data with
  | TLHandler h -> Some (PExpr h.ast)
  | TLFunc f -> Some (PExpr f.ufAST)
  | _ -> None

let replace (p : pointerData) (replacement : pointerData) (tl : toplevel) :
    toplevel =
  let ha () = tl |> asHandler |> deOption "TL.replace" in
  let fn () = tl |> asUserFunction |> deOption "TL.replace" in
  let id = P.toID p in
  let astReplace () =
    match tl.data with
    | TLHandler h ->
        let newAST = AST.replace p replacement h.ast in
        {tl with data= TLHandler {h with ast= newAST}}
    | TLFunc f ->
        let newAST = AST.replace p replacement f.ufAST in
        {tl with data= TLFunc {f with ufAST= newAST}}
    | _ -> impossible ("no AST here", tl.data)
  in
  let specHeaderReplace bo =
    let h = ha () in
    let newSpec = SpecHeaders.replace id bo h.spec in
    {tl with data= TLHandler {h with spec= newSpec}}
  in
  let fnMetadataReplace () =
    let f = fn () in
    let newF = Fns.replaceMetadataField p replacement f in
    {tl with data= TLFunc newF}
  in
  match replacement with
  | PVarBind vb -> astReplace ()
  | PField _ -> astReplace ()
  | PKey _ -> astReplace ()
  | PExpr _ -> astReplace ()
  | PEventName en -> specHeaderReplace en
  | PEventModifier em -> specHeaderReplace em
  | PEventSpace es -> specHeaderReplace es
  | PDBColType tipe -> tl
  | PDBColName _ -> tl
  | PFFMsg bo -> (
    match tl.data with
    | TLHandler h ->
        let spec2 = SpecHeaders.replace id bo h.spec in
        let ast = AST.replace p replacement h.ast in
        {tl with data= TLHandler {h with spec= spec2; ast}}
    | TLFunc f ->
        let ast = AST.replace p replacement f.ufAST in
        {tl with data= TLFunc {f with ufAST= ast}}
    | _ -> tl )
  | PFnName _ -> fnMetadataReplace ()
  | PParamName _ -> fnMetadataReplace ()
  | PParamTipe _ -> fnMetadataReplace ()
  | PPattern _ -> astReplace ()

let delete (tl : toplevel) (p : pointerData) (newID : id) : toplevel =
  let replacement = P.emptyD_ newID (P.typeOf p) in
  replace p replacement tl

let all (m : model) : toplevel list =
  m.toplevels @ List.map (ufToTL m) m.userFunctions

let get (m : model) (id : tlid) : toplevel option =
  let tls = all m in
  List.find (fun tl -> tl.id = id) tls

let getTL (m : model) (id : tlid) : toplevel = get m id |> deOption "getTL"

let find (tl : toplevel) (id : id) : pointerData option =
  allData tl
  |> List.filter (fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  |> List.head

let findExn (tl : toplevel) (id : id) : pointerData =
  find tl id |> deOption "findExn"
