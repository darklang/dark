module B = Blank
module P = Pointer
open Prelude
open Types

let ufpToP ufp =
  match (ufp.name, ufp.tipe) with
  | F (_, name), F (_, tipe) ->
      { name
      ; tipe
      ; block_args= ufp.block_args
      ; optional= ufp.optional
      ; description= ufp.description }
      |> Just
  | _ -> Nothing

let ufmToF ufm =
  let ps = List.filterMap ufpToP ufm.parameters in
  let sameLength = List.length ps == List.length ufm.parameters in
  match (ufm.name, ufm.returnTipe, sameLength) with
  | F (_, name), F (_, tipe), true ->
      { name
      ; parameters= ps
      ; description= ufm.description
      ; returnTipe= tipe
      ; infix= ufm.infix
      ; previewExecutionSafe= false
      ; deprecated= false }
      |> Just
  | _ -> Nothing

let find m id = Port.getBy (fun f -> id == f.tlid) m.userFunctions

let upsert m f =
  match find m f.tlid with
  | Just old ->
      { m with
        userFunctions=
          m.userFunctions
          |> List.filter (fun uf -> uf.tlid /= old.tlid)
          |> List.cons f }
  | Nothing -> {m with userFunctions= f :: m.userFunctions}

let findExn m id = find m id |> Option.getExn "Functions.findExn"

let sameName name uf =
  match uf.metadata.name with F (_, n) -> n == name | _ -> false

let findByName m s = Port.getBy (sameName s) m.userFunctions

let findByNameExn m s =
  findByName m s |> Option.getExn "Functions.findByNameExn"

let paramData ufp = [PParamName ufp.name; PParamTipe ufp.tipe]

let allParamData uf = List.concat (List.map paramData uf.metadata.parameters)

let allData uf =
  [PFnName uf.metadata.name] ++ allParamData uf ++ AST.allData uf.ast

let replaceFnName search replacement uf =
  let metadata = uf.metadata in
  let sId = P.toID search in
  if B.toID metadata.name == sId then
    let newMetadata =
      match replacement with
      | PFnName new_ -> {metadata with name= B.replace sId new_ metadata.name}
      | _ -> metadata
    in
    {uf with metadata= newMetadata}
  else uf

let replaceParamName search replacement uf =
  let metadata = uf.metadata in
  let sId = P.toID search in
  let paramNames =
    uf |> allParamData
    |> List.filterMap (fun p ->
           match p with PParamName n -> Just n | _ -> Nothing )
  in
  if List.any (fun p -> B.toID p == sId) paramNames then
    let newMetadata =
      match replacement with
      | PParamName new_ ->
          let newP =
            metadata.parameters
            |> List.map (fun p -> {p with name= B.replace sId new_ p.name})
          in
          {metadata with parameters= newP}
      | _ -> metadata
    in
    let newBody =
      let sContent =
        match search with
        | PParamName d -> B.toMaybe d
        | _ -> impossible search
      in
      let rContent =
        match replacement with
        | PParamName d -> B.toMaybe d
        | _ -> impossible replacement
      in
      let transformUse rep old =
        match old with
        | PExpr (F (_, _)) -> PExpr (F (gid (), Variable rep))
        | _ -> impossible old
      in
      match (sContent, rContent) with
      | Just o, Just r ->
          let uses = AST.uses o uf.ast |> List.map PExpr in
          List.foldr
            (fun use acc -> AST.replace use (transformUse r use) acc)
            uf.ast uses
      | _ -> uf.ast
    in
    {uf with metadata= newMetadata; ast= newBody}
  else uf

let replaceParamTipe search replacement uf =
  let metadata = uf.metadata in
  let sId = P.toID search in
  let paramTipes =
    uf |> allParamData
    |> List.filterMap (fun p ->
           match p with PParamTipe t -> Just t | _ -> Nothing )
  in
  if List.any (fun p -> B.toID p == sId) paramTipes then
    let newMetadata =
      match replacement with
      | PParamTipe new_ ->
          let newP =
            metadata.parameters
            |> List.map (fun p -> {p with tipe= B.replace sId new_ p.tipe})
          in
          {metadata with parameters= newP}
      | _ -> metadata
    in
    {uf with metadata= newMetadata}
  else uf

let replaceMetadataField old new_ uf =
  uf |> replaceFnName old new_ |> replaceParamName old new_
  |> replaceParamTipe old new_

let extend uf =
  let newParam =
    { name= B.new_ ()
    ; tipe= B.new_ ()
    ; block_args= []
    ; optional= false
    ; description= "" }
  in
  let metadata = uf.metadata in
  let newMetadata =
    {metadata with parameters= uf.metadata.parameters ++ [newParam]}
  in
  {uf with metadata= newMetadata}

let removeParameter uf ufp =
  let metadata = uf.metadata in
  let params = List.filter (fun p -> p /= ufp) metadata.parameters in
  let newM = {metadata with parameters= params} in
  {uf with metadata= newM}
