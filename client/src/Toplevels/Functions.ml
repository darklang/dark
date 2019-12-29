open Tc
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

let allNames (fns : userFunction TLIDDict.t) : string list =
  fns |> TD.filterMapValues ~f:(fun fn -> B.toMaybe fn.ufMetadata.ufmName)


let toID (uf : userFunction) : tlid = uf.ufTLID

let upsert (m : model) (userFunction : userFunction) : model =
  { m with
    userFunctions =
      TD.insert ~tlid:userFunction.ufTLID ~value:userFunction m.userFunctions }


let update (m : model) ~(tlid : tlid) ~(f : userFunction -> userFunction) :
    model =
  {m with userFunctions = TD.updateIfPresent ~tlid ~f m.userFunctions}


let remove (m : model) (userFunction : userFunction) : model =
  {m with userFunctions = TD.remove ~tlid:userFunction.ufTLID m.userFunctions}


let fromList (ufs : userFunction list) : userFunction TLIDDict.t =
  ufs |> List.map ~f:(fun uf -> (uf.ufTLID, uf)) |> TLIDDict.fromList


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


let sameName (name : string) (uf : userFunction) : bool =
  match uf.ufMetadata.ufmName with F (_, n) -> n = name | _ -> false


let findByName (m : model) (s : string) : userFunction option =
  List.find ~f:(sameName s) (TLIDDict.values m.userFunctions)


let paramData (ufp : userFunctionParameter) : blankOrData list =
  [PParamName ufp.ufpName; PParamTipe ufp.ufpTipe]


let allParamData (uf : userFunction) : blankOrData list =
  List.concat (List.map ~f:paramData uf.ufMetadata.ufmParameters)


let allData (uf : userFunction) : blankOrData list =
  [PFnName uf.ufMetadata.ufmName] @ allParamData uf


let replaceFnName
    (search : blankOrData) (replacement : blankOrData) (uf : userFunction) :
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
         match p with PParamName (F (_, n)) -> Some n | _ -> None)


let replaceParamName
    (search : blankOrData) (replacement : blankOrData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  let paramNames =
    uf
    |> allParamData
    |> List.filterMap ~f:(fun p ->
           match p with PParamName n -> Some n | _ -> None)
  in
  if List.any ~f:(fun p -> B.toID p = sId) paramNames
  then
    let newMetadata =
      match replacement with
      | PParamName new_ ->
          let newP =
            metadata.ufmParameters
            |> List.map ~f:(fun p ->
                   {p with ufpName = B.replace sId new_ p.ufpName})
          in
          {metadata with ufmParameters = newP}
      | _ ->
          metadata
    in
    let newBody =
      match (search, replacement) with
      | PParamName (F (_, oldName)), PParamName (F (_, newName)) ->
          FluidExpression.renameVariableUses ~oldName ~newName uf.ufAST
      | _ ->
          uf.ufAST
    in
    {uf with ufMetadata = newMetadata; ufAST = newBody}
  else uf


let replaceParamTipe
    (search : blankOrData) (replacement : blankOrData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  let paramTipes =
    uf
    |> allParamData
    |> List.filterMap ~f:(fun p ->
           match p with PParamTipe t -> Some t | _ -> None)
  in
  if List.any ~f:(fun p -> B.toID p = sId) paramTipes
  then
    let newMetadata =
      match replacement with
      | PParamTipe new_ ->
          let newP =
            metadata.ufmParameters
            |> List.map ~f:(fun p ->
                   {p with ufpTipe = B.replace sId new_ p.ufpTipe})
          in
          {metadata with ufmParameters = newP}
      | _ ->
          metadata
    in
    {uf with ufMetadata = newMetadata}
  else uf


let usesOfTipe (tipename : string) (version : int) (uf : userFunction) :
    blankOrData list =
  uf
  |> allParamData
  |> List.filterMap ~f:(fun p ->
         match p with
         | PParamTipe (F (_, TUserType (n, v))) as pd
           when n = tipename && v = version ->
             Some pd
         | _ ->
             None)


let replaceMetadataField
    (old : blankOrData) (new_ : blankOrData) (uf : userFunction) : userFunction
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


let idOfLastBlankor (f : userFunction) : id =
  List.last f.ufMetadata.ufmParameters
  |> Option.andThen ~f:(fun p -> Some (B.toID p.ufpTipe))
  |> Option.withDefault ~default:(B.toID f.ufMetadata.ufmName)
