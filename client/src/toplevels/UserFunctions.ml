open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let allNames (fns : userFunction TLIDDict.t) : string list =
  fns |> TD.filterMapValues ~f:(fun fn -> B.toOption fn.ufMetadata.ufmName)


let toID (uf : userFunction) : TLID.t = uf.ufTLID

let upsert (m : model) (userFunction : userFunction) : model =
  { m with
    userFunctions =
      TD.insert ~tlid:userFunction.ufTLID ~value:userFunction m.userFunctions }


let update (m : model) ~(tlid : TLID.t) ~(f : userFunction -> userFunction) :
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
      ; fnPreviewSafety = Unsafe
      ; fnDeprecated = false
      ; fnIsSupportedInQuery = false
      ; fnOrigin = UserFunction }
      |> fun x -> Some x
  | _ ->
      None


let sameName (name : string) (uf : userFunction) : bool =
  match uf.ufMetadata.ufmName with F (_, n) -> n = name | _ -> false


let paramData (ufp : userFunctionParameter) : blankOrData list =
  [PParamName ufp.ufpName; PParamTipe ufp.ufpTipe]


let allParamData (uf : userFunction) : blankOrData list =
  List.concat (List.map ~f:paramData uf.ufMetadata.ufmParameters)


let blankOrData (uf : userFunction) : blankOrData list =
  PFnName uf.ufMetadata.ufmName
  :: PFnReturnTipe uf.ufMetadata.ufmReturnTipe
  :: allParamData uf


let replaceFnReturn
    (search : blankOrData) (replacement : blankOrData) (uf : userFunction) :
    userFunction =
  let metadata = uf.ufMetadata in
  let sId = P.toID search in
  if B.toID metadata.ufmReturnTipe = sId
  then
    let newMetadata =
      match replacement with
      | PFnReturnTipe new_ ->
          { metadata with
            ufmReturnTipe = B.replace sId new_ metadata.ufmReturnTipe }
      | _ ->
          metadata
    in
    {uf with ufMetadata = newMetadata}
  else uf


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
          uf.ufAST
          |> FluidAST.map
               ~f:(FluidExpression.renameVariableUses ~oldName ~newName)
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
  |> replaceFnReturn old new_
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


let idOfLastBlankor (f : userFunction) : ID.t =
  List.last f.ufMetadata.ufmParameters
  |> Option.andThen ~f:(fun p -> Some (B.toID p.ufpTipe))
  |> Option.withDefault ~default:(B.toID f.ufMetadata.ufmName)


(* Converts inputValueDict to executeFunctionAPIParams.efpArgs *)
let inputToArgs (f : userFunction) (input : inputValueDict) : dval list =
  let default = DIncomplete SourceNone in
  f.ufMetadata.ufmParameters
  |> List.map ~f:(fun p ->
         match p.ufpName with
         | F (_, name) ->
             StrDict.get ~key:name input |> Option.withDefault ~default
         | _ ->
             default)


let canDelete (usedInRefs : toplevel list) (tlid : TLID.t) : bool =
  (* Allow deletion if the only callers are itself or there are no references at all.
    List.all returns true if the list is empty.
  *)
  usedInRefs
  |> List.all ~f:(function TLFunc f when f.ufTLID = tlid -> true | _ -> false)
