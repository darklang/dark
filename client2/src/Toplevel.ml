open Tea
open! Porting
module B = Blank
module Fns = Functions
module P = Pointer
open Prelude
open Types

let all m = m.toplevels ^ List.map (ufToTL m) m.userFunctions

let getTL m id = get m id |> Option.getExn "getTL"

let get m id =
  let tls = all m in
  List.find (fun tl -> tl.id = id) tls

let name tl =
  match tl.data with
  | TLHandler h -> "H: " ^ (h.spec.name |> B.toMaybe |> Option.withDefault "")
  | TLDB db -> "DB: " ^ db.name
  | TLFunc f ->
      "Func: " ^ (f.metadata.name |> B.toMaybe |> Option.withDefault "")

let upsertByTLID tls tl = removeByTLID tls [tl] ^ [tl]

let upsert m tl = {m with toplevels= upsertByTLID m.toplevels tl}

let upsertAllByTLID tls new_ =
  List.foldl (fun a b -> upsertByTLID b a) tls new_

let upsertAll m tls = List.foldl (fun a b -> upsert b a) m tls

let containsByTLID tls elem = List.find (fun tl -> tl.id = elem.id) tls <> None

let removeByTLID origTls toBeRemoved =
  List.filter (fun origTl -> not (containsByTLID toBeRemoved origTl)) origTls

let remove m tl = {m with toplevels= removeByTLID m.toplevels [tl]}

let updateByTLID tls tlid f =
  tls |> List.map (fun t -> if t.id <> tlid then t else f t)

let update m tlid f = {m with toplevels= updateByTLID m.toplevels tlid f}

let move tlid xOffset yOffset m = update m tlid (moveTL xOffset yOffset)

let moveTL xOffset yOffset tl =
  let newPos = {x= tl.pos.x + xOffset; y= tl.pos.y + yOffset} in
  {tl with pos= newPos}

let ufToTL m uf = {id= uf.tlid; pos= Defaults.centerPos; data= TLFunc uf}

let asUserFunction tl = match tl.data with TLFunc f -> Some f | _ -> None

let asHandler tl = match tl.data with TLHandler h -> Some h | _ -> None

let asDB tl = match tl.data with TLDB h -> Some h | _ -> None

let handlers tls = List.filterMap asHandler tls

let dbs tls = List.filterMap asDB tls

let spaceOfHandler h = SpecHeaders.spaceOf h.spec

let spaceOf tl = tl |> asHandler |> Option.map spaceOfHandler

let isHTTPHandler tl = tl |> spaceOf |> ( = ) (Some HSHTTP)

let toOp tl =
  match tl.data with
  | TLHandler h -> [SetHandler (tl.id, tl.pos, h)]
  | TLFunc fn -> [SetFunction fn]
  | _ -> impossible "This isn't how database ops work"

let isValidID tl id = List.member id (tl |> allData |> List.map P.toID)

let clonePointerData pd =
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
  | PDarkType dt -> todo ("clonePointerData", pd)
  | PDarkTypeField dt -> todo ("clonePointerData", pd)
  | PFFMsg msg -> PFFMsg (B.clone identity msg)
  | PFnName name_ -> PFnName (B.clone identity name_)
  | PParamName name_ -> PParamName (B.clone identity name_)
  | PParamTipe tipe -> PParamTipe (B.clone identity tipe)

let allBlanks tl = tl |> allData |> List.filter P.isBlank

let blanksWhere fn tl = tl |> allBlanks |> List.filter fn

let getNextBlank tl pred =
  match pred with
  | Some pred_ ->
      let ps = allData tl in
      let index = List.elemIndex pred_ ps |> Option.withDefault (-1) in
      let remaining = List.drop (index + 1) ps in
      let blanks = List.filter P.isBlank remaining in
      blanks |> List.head |> Option.orElse (firstBlank tl)
  | None -> firstBlank tl

let getPrevBlank tl next =
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

let firstBlank tl = tl |> allBlanks |> List.head

let lastBlank tl = tl |> allBlanks |> List.last

let siblings tl p =
  match tl.data with
  | TLHandler h ->
      let toplevels = SpecHeaders.allData h.spec ^ [PExpr h.ast] in
      if List.member p toplevels then toplevels
      else
        (AST.siblings p h.ast ^ SpecTypes.siblings p h.spec.types.input)
        ^ SpecTypes.siblings p h.spec.types.output
  | TLDB db -> DB.siblings p db
  | TLFunc f -> AST.siblings p f.ast

let getNextSibling tl p =
  siblings tl p |> Util.listNextWrap p |> Option.getExn "nextSibling"

let getPrevSibling tl p =
  siblings tl p |> Util.listPreviousWrap p |> Option.getExn "prevSibling"

let getParentOf tl p =
  match tl.data with
  | TLHandler h -> AST.parentOf_ (P.toID p) h.ast |> Option.map PExpr
  | TLFunc f -> AST.parentOf_ (P.toID p) f.ast |> Option.map PExpr
  | TLDB db ->
      db |> DB.astsFor
      |> List.map (AST.parentOf_ (P.toID p))
      |> Option.values |> List.head |> Option.map PExpr

let getChildrenOf tl pd =
  let pid = P.toID pd in
  let astChildren () =
    match tl.data with
    | TLHandler h -> AST.childrenOf pid h.ast
    | TLFunc f -> AST.childrenOf pid f.ast
    | TLDB db ->
        db |> DB.astsFor |> List.map (AST.childrenOf pid) |> List.concat
  in
  let specChildren () =
    let h = asHandler tl |> Option.getExn "getChildrenOf - spec" in
    SpecTypes.childrenOf (P.toID pd) h.spec.types.input
    ^ SpecTypes.childrenOf (P.toID pd) h.spec.types.output
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
  | PDarkType _ -> specChildren ()
  | PDarkTypeField d -> []
  | PFFMsg _ -> []
  | PFnName _ -> []
  | PParamName _ -> []
  | PParamTipe _ -> []

let firstChild tl id = getChildrenOf tl id |> List.head

let rootOf tl =
  match tl.data with
  | TLHandler h -> Some <| PExpr h.ast
  | TLFunc f -> Some <| PExpr f.ast
  | _ -> None

let replace p replacement tl =
  let ha () = tl |> asHandler |> Option.getExn "TL.replace" in
  let fn () = tl |> asUserFunction |> Option.getExn "TL.replace" in
  let id = P.toID p in
  let astReplace () =
    match tl.data with
    | TLHandler h ->
        let newAST = AST.replace p replacement h.ast in
        {tl with data= TLHandler {h with ast= newAST}}
    | TLFunc f ->
        let newAST = AST.replace p replacement f.ast in
        {tl with data= TLFunc {f with ast= newAST}}
    | _ -> impossible ("no AST here", tl.data)
  in
  let specTypeReplace () =
    let h = ha () in
    let newSpec = SpecTypes.replace p replacement h.spec in
    {tl with data= TLHandler {h with spec= newSpec}}
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
  | PDarkType _ -> specTypeReplace ()
  | PDarkTypeField _ -> specTypeReplace ()
  | PDBColType tipe -> tl
  | PDBColName _ -> tl
  | PFFMsg bo -> (
    match tl.data with
    | TLHandler h ->
        let spec2 = SpecHeaders.replace id bo h.spec in
        let ast = AST.replace p replacement h.ast in
        {tl with data= TLHandler {h with spec= spec2; ast}}
    | TLFunc f ->
        let ast = AST.replace p replacement f.ast in
        {tl with data= TLFunc {f with ast}}
    | _ -> tl )
  | PFnName _ -> fnMetadataReplace ()
  | PParamName _ -> fnMetadataReplace ()
  | PParamTipe _ -> fnMetadataReplace ()

let delete tl p newID =
  let replacement = P.emptyD_ newID (P.typeOf p) in
  replace p replacement tl

let allData tl =
  match tl.data with
  | TLHandler h ->
      ( (SpecHeaders.allData h.spec ^ SpecTypes.allData h.spec.types.input)
      ^ AST.allData h.ast )
      ^ SpecTypes.allData h.spec.types.output
  | TLDB db -> DB.allData db
  | TLFunc f -> Fns.allData f

let findExn tl id = find tl id |> Option.getExn "findExn"

let find tl id =
  allData tl
  |> List.filter (fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  |> List.head
