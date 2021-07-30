open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let blankOrData (t : userTipe) : blankOrData list =
  let namePointer = PTypeName t.utName in
  let definitionPointers =
    match t.utDefinition with
    | UTRecord fields ->
        List.fold
          ~initial:[]
          ~f:(fun acc f ->
            acc @ [PTypeFieldName f.urfName; PTypeFieldTipe f.urfTipe])
          fields
  in
  namePointer :: definitionPointers


let toID (ut : userTipe) : TLID.t = ut.utTLID

let upsert (m : model) (ut : userTipe) : model =
  {m with userTipes = Map.add ~key:ut.utTLID ~value:ut m.userTipes}


let update (m : model) ~(tlid : TLID.t) ~(f : userTipe -> userTipe) : model =
  {m with userTipes = Map.updateIfPresent ~key:tlid ~f m.userTipes}


let remove (m : model) (ut : userTipe) : model =
  {m with userTipes = Map.remove ~key:ut.utTLID m.userTipes}


let fromList (uts : userTipe list) : userTipe TLIDDict.t =
  uts |> List.map ~f:(fun ut -> (ut.utTLID, ut)) |> TLIDDict.fromList


let allNames (tipes : userTipe TLIDDict.t) : string list =
  tipes |> Map.filterMapValues ~f:(fun t -> B.toOption t.utName)


let toTUserType (tipe : userTipe) : tipe option =
  tipe.utName
  |> B.toOption
  |> Option.map ~f:(fun n -> TUserType (n, tipe.utVersion))


let replaceDefinitionElement
    (old : blankOrData) (new_ : blankOrData) (tipe : userTipe) : userTipe =
  let sId = P.toID old in
  match tipe.utDefinition with
  | UTRecord fields ->
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
            else f)
          fields
      in
      {tipe with utDefinition = UTRecord newFields}


let replaceTypeName (old : blankOrData) (new_ : blankOrData) (tipe : userTipe) :
    userTipe =
  let sId = P.toID old in
  if B.toID tipe.utName = sId
  then
    match new_ with
    | PTypeName new_ ->
        {tipe with utName = B.replace sId new_ tipe.utName}
    | _ ->
        tipe
  else tipe


let replace (old : blankOrData) (new_ : blankOrData) (tipe : userTipe) :
    userTipe =
  tipe |> replaceTypeName old new_ |> replaceDefinitionElement old new_


let extend (tipe : userTipe) : userTipe =
  match tipe.utDefinition with
  | UTRecord fields ->
      let newFields = fields @ [{urfName = B.new_ (); urfTipe = B.new_ ()}] in
      {tipe with utDefinition = UTRecord newFields}


let removeField (tipe : userTipe) (field : userRecordField) : userTipe =
  match tipe.utDefinition with
  | UTRecord fields ->
      let newFields = List.filter ~f:(fun f -> field <> f) fields in
      {tipe with utDefinition = UTRecord newFields}
