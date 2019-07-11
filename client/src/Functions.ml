open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

let allNames (fns : userFunction list) : string list =
  List.filter_map ~f:(fun fn -> B.toMaybe fn.ufMetadata.ufmName) fns


let ufpToP (ufp : userFunctionParameter) : parameter option =
  match (ufp.ufpName, ufp.ufpTipe) with
  | F (_, name), F (_, tipe) ->
      { paramName = name
      ; paramTipe = tipe
      ; paramBlock_args = ufp.ufpBlock_args
      ; paramOptional = ufp.ufpOptional
      ; paramDescription = ufp.ufpDescription }
      |> fun x -> Some x
  | _ ->
      None


let ufmToF (ufm : userFunctionMetadata) : function_ option =
  let ps = List.filterMap ~f:ufpToP ufm.ufmParameters in
  let sameLength = List.length ps = List.length ufm.ufmParameters in
  match (ufm.ufmName, ufm.ufmReturnTipe, sameLength) with
  | F (_, name), F (_, tipe), true ->
      { fnName = name
      ; fnParameters = ps
      ; fnDescription = ufm.ufmDescription
      ; fnReturnTipe = tipe
      ; fnInfix = ufm.ufmInfix
      ; fnPreviewExecutionSafe = false
      ; fnDeprecated = false }
      |> fun x -> Some x
  | _ ->
      None


let find (m : model) (id : tlid) : userFunction option =
  List.find ~f:(fun f -> id = f.ufTLID) m.userFunctions


let findExn (m : model) (id : tlid) : userFunction =
  find m id |> deOption "Functions.findExn"


let upsert (m : model) (f : userFunction) : model =
  match find m f.ufTLID with
  | Some old ->
      { m with
        userFunctions =
          m.userFunctions
          |> List.filter ~f:(fun uf -> uf.ufTLID <> old.ufTLID)
          |> List.cons f }
  | None ->
      {m with userFunctions = f :: m.userFunctions}


let containsByTLID (fns : userFunction list) (elem : userFunction) : bool =
  List.find ~f:(fun fn -> fn.ufTLID = elem.ufTLID) fns <> None


let removeByTLID
    ~(toBeRemoved : userFunction list) (origfns : userFunction list) :
    userFunction list =
  List.filter
    ~f:(fun origfn -> not (containsByTLID toBeRemoved origfn))
    origfns


let upsertByTLID (fns : userFunction list) (fn : userFunction) :
    userFunction list =
  removeByTLID fns ~toBeRemoved:[fn] @ [fn]


let upsertAllByTLID (fns : userFunction list) ~(newFns : userFunction list) :
    userFunction list =
  List.foldl ~f:(fun fn fns -> upsertByTLID fns fn) ~init:fns newFns


let sameName (name : string) (uf : userFunction) : bool =
  match uf.ufMetadata.ufmName with F (_, n) -> n = name | _ -> false


let findByName (m : model) (s : string) : userFunction option =
  List.find ~f:(sameName s) m.userFunctions


let findByNameExn (m : model) (s : string) : userFunction =
  findByName m s |> deOption "Functions.findByNameExn"


let paramData (ufp : userFunctionParameter) : pointerData list =
  [PParamName ufp.ufpName; PParamTipe ufp.ufpTipe]


let allParamData (uf : userFunction) : pointerData list =
  List.concat (List.map ~f:paramData uf.ufMetadata.ufmParameters)


let allData (uf : userFunction) : pointerData list =
  [PFnName uf.ufMetadata.ufmName] @ allParamData uf @ AST.allData uf.ufAST


let replaceFnName
    (search : pointerData) (replacement : pointerData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  if B.toID metadata.ufmName = sId
  then
    let newMetadata =
      match replacement with
      | PFnName new_ ->
          {metadata with ufmName = B.replace sId new_ metadata.ufmName}
      | _ ->
          metadata
    in
    {uf with ufMetadata = newMetadata}
  else uf

let allParamNames (uf : userFunction) : string list =
  uf
  |> allParamData
  |> List.filterMap ~f:(fun p ->
          match p with PParamName (F (_, n)) -> Some n | _ -> None )

let replaceParamName
    (search : pointerData) (replacement : pointerData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  let paramNames =
    uf
    |> allParamData
    |> List.filterMap ~f:(fun p ->
           match p with PParamName n -> Some n | _ -> None )
  in
  if List.any ~f:(fun p -> B.toID p = sId) paramNames
  then
    let newMetadata =
      match replacement with
      | PParamName new_ ->
          let newP =
            metadata.ufmParameters
            |> List.map ~f:(fun p ->
                   {p with ufpName = B.replace sId new_ p.ufpName} )
          in
          {metadata with ufmParameters = newP}
      | _ ->
          metadata
    in
    let newBody =
      let sContent =
        match search with
        | PParamName d ->
            B.toMaybe d
        | _ ->
            impossible search
      in
      let rContent =
        match replacement with
        | PParamName d ->
            B.toMaybe d
        | _ ->
            impossible replacement
      in
      let transformUse rep old =
        match old with
        | PExpr (F (_, _)) ->
            PExpr (F (gid (), Variable rep))
        | _ ->
            impossible old
      in
      match (sContent, rContent) with
      | Some o, Some r ->
          let uses = AST.uses o uf.ufAST |> List.map ~f:(fun x -> PExpr x) in
          List.foldr
            ~f:(fun use acc -> AST.replace use (transformUse r use) acc)
            ~init:uf.ufAST
            uses
      | _ ->
          uf.ufAST
    in
    {uf with ufMetadata = newMetadata; ufAST = newBody}
  else uf


let replaceParamTipe
    (search : pointerData) (replacement : pointerData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  let paramTipes =
    uf
    |> allParamData
    |> List.filterMap ~f:(fun p ->
           match p with PParamTipe t -> Some t | _ -> None )
  in
  if List.any ~f:(fun p -> B.toID p = sId) paramTipes
  then
    let newMetadata =
      match replacement with
      | PParamTipe new_ ->
          let newP =
            metadata.ufmParameters
            |> List.map ~f:(fun p ->
                   {p with ufpTipe = B.replace sId new_ p.ufpTipe} )
          in
          {metadata with ufmParameters = newP}
      | _ ->
          metadata
    in
    {uf with ufMetadata = newMetadata}
  else uf


let usesOfTipe (tipename : string) (version : int) (uf : userFunction) :
    pointerData list =
  uf
  |> allParamData
  |> List.filterMap ~f:(fun p ->
         match p with
         | PParamTipe (F (_, TUserType (n, v))) as pd
           when n = tipename && v = version ->
             Some pd
         | _ ->
             None )


let replaceMetadataField
    (old : pointerData) (new_ : pointerData) (uf : userFunction) : userFunction
    =
  uf
  |> replaceFnName old new_
  |> replaceParamName old new_
  |> replaceParamTipe old new_


let extend (uf : userFunction) : userFunction =
  let newParam =
    { ufpName = B.new_ ()
    ; ufpTipe = B.new_ ()
    ; ufpBlock_args = []
    ; ufpOptional = false
    ; ufpDescription = "" }
  in
  let metadata = uf.ufMetadata in
  let newMetadata =
    {metadata with ufmParameters = uf.ufMetadata.ufmParameters @ [newParam]}
  in
  {uf with ufMetadata = newMetadata}


let removeParameter (uf : userFunction) (ufp : userFunctionParameter) :
    userFunction =
  let metadata = uf.ufMetadata in
  let params = List.filter ~f:(fun p -> p <> ufp) metadata.ufmParameters in
  let newM = {metadata with ufmParameters = params} in
  {uf with ufMetadata = newM}


let findByNameInList (name : string) (functions : function_ list) : function_ =
  functions
  |> List.find ~f:(fun f -> f.fnName = name)
  |> Option.withDefault
       ~default:
         { fnName = "fnLookupError"
         ; fnParameters = []
         ; fnDescription = "default, fn error"
         ; fnReturnTipe = TError
         ; fnPreviewExecutionSafe = true
         ; fnInfix = false
         ; fnDeprecated = false }
