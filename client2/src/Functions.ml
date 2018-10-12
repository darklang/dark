open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
open Types

let ufpToP (ufp : userFunctionParameter) : parameter option =
  match (ufp.name, ufp.tipe) with
  | F (_, name), F (_, tipe) ->
      { name
      ; tipe
      ; block_args= ufp.block_args
      ; optional= ufp.optional
      ; description= ufp.description }
      |> fun x -> Some x
  | _ -> None

let ufmToF (ufm : userFunctionMetadata) : function_ option =
  let ps = List.filterMap ufpToP ufm.parameters in
  let sameLength = List.length ps = List.length ufm.parameters in
  match (ufm.name, ufm.returnTipe, sameLength) with
  | F (_, name), F (_, tipe), true ->
      { name
      ; parameters= ps
      ; description= ufm.description
      ; returnTipe= tipe
      ; infix= ufm.infix
      ; previewExecutionSafe= false
      ; deprecated= false }
      |> fun x -> Some x
  | _ -> None

let find (m : model) (id : tlid) : userFunction option =
  List.find (fun f -> id = f.tlid) m.userFunctions

let upsert (m : model) (f : userFunction) : model =
  match find m f.tlid with
  | Some old ->
      { m with
        userFunctions=
          m.userFunctions
          |> List.filter (fun uf -> uf.tlid <> old.tlid)
          |> List.cons f }
  | None -> {m with userFunctions= f :: m.userFunctions}

let findExn (m : model) (id : tlid) : userFunction =
  find m id |> deOption "Functions.findExn"

let sameName (name : string) (uf : userFunction) : bool =
  match uf.metadata.name with F (_, n) -> n = name | _ -> false

let findByName (m : model) (s : string) : userFunction option =
  List.find (sameName s) m.userFunctions

let findByNameExn (m : model) (s : string) : userFunction =
  findByName m s |> deOption "Functions.findByNameExn"

let paramData (ufp : userFunctionParameter) : pointerData list =
  [PParamName ufp.name; PParamTipe ufp.tipe]

let allParamData (uf : userFunction) : pointerData list =
  List.concat (List.map paramData uf.metadata.parameters)

let rec allData (uf : userFunction) : pointerData list =
  [PFnName uf.metadata.name] ^ allParamData uf ^ AST.allData uf.ast

let replaceFnName (search : pointerData) (replacement : pointerData)
    (uf : userFunction) : userFunction =
  let metadata = uf.metadata in
  let sId = P.toID search in
  if B.toID metadata.name = sId then
    let newMetadata =
      match replacement with
      | PFnName new_ -> {metadata with name= B.replace sId new_ metadata.name}
      | _ -> metadata
    in
    {uf with metadata= newMetadata}
  else uf

let replaceParamName (search : pointerData) (replacement : pointerData)
    (uf : userFunction) : userFunction =
  let metadata = uf.metadata in
  let sId = P.toID search in
  let paramNames =
    uf |> allParamData
    |> List.filterMap (fun p ->
           match p with PParamName n -> Some n | _ -> None )
  in
  if List.any (fun p -> B.toID p = sId) paramNames then
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
      | Some o, Some r ->
          let uses = AST.uses o uf.ast |> List.map PExpr in
          List.foldr
            (fun use acc -> AST.replace use (transformUse r use) acc)
            uf.ast uses
      | _ -> uf.ast
    in
    {uf with metadata= newMetadata; ast= newBody}
  else uf

let replaceParamTipe (search : pointerData) (replacement : pointerData)
    (uf : userFunction) : userFunction =
  let metadata = uf.metadata in
  let sId = P.toID search in
  let paramTipes =
    uf |> allParamData
    |> List.filterMap (fun p ->
           match p with PParamTipe t -> Some t | _ -> None )
  in
  if List.any (fun p -> B.toID p = sId) paramTipes then
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

let replaceMetadataField (old : pointerData) (new_ : pointerData)
    (uf : userFunction) : userFunction =
  uf |> replaceFnName old new_ |> replaceParamName old new_
  |> replaceParamTipe old new_

let extend (uf : userFunction) : userFunction =
  let newParam =
    { name= B.new_ ()
    ; tipe= B.new_ ()
    ; block_args= []
    ; optional= false
    ; description= "" }
  in
  let metadata = uf.metadata in
  let newMetadata =
    {metadata with parameters= uf.metadata.parameters ^ [newParam]}
  in
  {uf with metadata= newMetadata}

let removeParameter (uf : userFunction) (ufp : userFunctionParameter) :
    userFunction =
  let metadata = uf.metadata in
  let params = List.filter (fun p -> p <> ufp) metadata.parameters in
  let newM = {metadata with parameters= params} in
  {uf with metadata= newM}
