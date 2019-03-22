open Tc
open Types

(* Dark *)
module B = Blank
module P = Pointer

let allData (t : userTipe) : pointerData list =
  let namePointer = PTypeName t.utName in
  let definitionPointers =
    match t.utDefinition with UTRecord fields ->
      List.foldl
        ~init:[]
        ~f:(fun f acc ->
          acc @ [PTypeFieldName f.urfName; PTypeFieldTipe f.urfTipe] )
        fields
  in
  namePointer :: definitionPointers


let allNames (tipes : userTipe list) : string list =
  tipes |> List.filter_map ~f:(fun t -> B.toMaybe t.utName)


let find (m : model) (id : tlid) : userTipe option =
  List.find ~f:(fun t -> id = t.utTLID) m.userTipes


let upsert (m : model) (t : userTipe) : model =
  match find m t.utTLID with
  | Some old ->
      { m with
        userTipes =
          m.userTipes
          |> List.filter ~f:(fun ut -> ut.utTLID <> old.utTLID)
          |> List.cons t }
  | None ->
      {m with userTipes = t :: m.userTipes}


let containsByTLID (tipes : userTipe list) (elem : userTipe) : bool =
  List.find ~f:(fun t -> t.utTLID = elem.utTLID) tipes <> None


let removeByTLID ~(toBeRemoved : userTipe list) (origTipes : userTipe list) :
    userTipe list =
  List.filter ~f:(fun orig -> not (containsByTLID toBeRemoved orig)) origTipes


let upsertByTLID (tipes : userTipe list) (t : userTipe) : userTipe list =
  removeByTLID tipes ~toBeRemoved:[t] @ [t]


let upsertAllByTLID (tipes : userTipe list) ~(newTipes : userTipe list) :
    userTipe list =
  List.foldl ~f:(fun t acc -> upsertByTLID acc t) ~init:tipes newTipes


let replaceDefinitionElement
    (old : pointerData) (new_ : pointerData) (tipe : userTipe) : userTipe =
  let sId = P.toID old in
  match tipe.utDefinition with UTRecord fields ->
    let newFields =
      List.map
        ~f:(fun f ->
          if B.toID f.urfName = sId
          then
            match new_ with
            | PTypeFieldName new_ ->
                {f with urfName = B.replace sId new_ f.urfName}
            | _ ->
                f
          else if B.toID f.urfTipe = sId
          then
            match new_ with
            | PTypeFieldTipe new_ ->
                {f with urfTipe = B.replace sId new_ f.urfTipe}
            | _ ->
                f
          else f )
        fields
    in
    {tipe with utDefinition = UTRecord newFields}


let replaceTypeName (old : pointerData) (new_ : pointerData) (tipe : userTipe)
    : userTipe =
  let sId = P.toID old in
  if B.toID tipe.utName = sId
  then
    match new_ with
    | PTypeName new_ ->
        {tipe with utName = B.replace sId new_ tipe.utName}
    | _ ->
        tipe
  else tipe


let replace (old : pointerData) (new_ : pointerData) (tipe : userTipe) :
    userTipe =
  tipe |> replaceTypeName old new_ |> replaceDefinitionElement old new_


let extend (tipe : userTipe) : userTipe =
  match tipe.utDefinition with UTRecord fields ->
    let newFields = fields @ [{urfName = B.new_ (); urfTipe = B.new_ ()}] in
    {tipe with utDefinition = UTRecord newFields}


let removeField (tipe : userTipe) (field : userRecordField) : userTipe =
  match tipe.utDefinition with UTRecord fields ->
    let newFields = List.filter ~f:(fun f -> field <> f) fields in
    {tipe with utDefinition = UTRecord newFields}
