open Tc
open Types

type codeRef = tlid * id

let keyForHandlerSpec (space : string blankOr) (name : string blankOr) : string
    =
  let space_ = match space with F (_, s) -> s | Blank _ -> "_" in
  let name_ = match name with F (_, n) -> n | Blank _ -> "_" in
  space_ ^ ":" ^ name_

let findUsagesInAST (tlid_ : tlid) (databases : tlid StrDict.t) (handlers : tlid StrDict.t) (pointers: pointerData list) : usage list =
  pointers
  |> List.filterMap
    ~f:(fun pd ->
        match pd with
        | PExpr (F (id, Variable name)) ->
            Debug.loG "Variable" name;
            StrDict.get ~key:name databases
            |> Option.andThen ~f:(fun tlid ->
                  Some (tlid_, tlid, Some id)
            )
        | PExpr
            (F
              ( id
              , FnCall
                  ( F (_, "emit")
                  , [_; F (sid, Value space_); F (nid, Value name_)]
                  , _ ) )) ->
            let name = Util.removeQuotes name_ in
            let space = Util.removeQuotes space_ in
            let key =
              keyForHandlerSpec (F (sid, space)) (F (nid, name))
            in
            StrDict.get ~key handlers
            |> Option.andThen ~f:(fun tlid ->
                  Some (tlid_, tlid, Some id )
            )
        | _ ->
            None )
  |> List.uniqueBy ~f:(fun (_, TLID tlid, _) -> tlid)

let tlidStrOfReference (r : tlReference) : string =
  match r with
  | OutReferenceDB (TLID tlid, _, _, _) ->
      tlid
  | OutReferenceEvent (TLID tlid, _, _, _) ->
      tlid
  | InReferenceHandler (TLID tlid, _, _, _) ->
      tlid
  | InReferenceFunction (TLID tlid, _, _) ->
      tlid


let idOfReference (r : tlReference) : id option =
  match r with
  | OutReferenceDB (_, _, _, id) ->
      Some id
  | OutReferenceEvent (_, _, _, id) ->
      Some id
  | InReferenceHandler _ ->
      None
  | InReferenceFunction _ ->
      None


let shouldUpdateReferences (ops : op list) =
  List.any
    ~f:(fun op ->
      match op with
      | SetHandler _ | SetExpr _ | SetFunction _ ->
          true
      | CreateDB _
      | AddDBCol _
      | SetDBColName _
      | SetDBColType _
      | DeleteTL _
      | MoveTL _
      | TLSavepoint _
      | UndoTL _
      | RedoTL _
      | DeleteFunction _
      | ChangeDBColName _
      | ChangeDBColType _
      | DeprecatedInitDbm _
      | CreateDBMigration _
      | AddDBColToDBMigration _
      | SetDBColNameInDBMigration _
      | SetDBColTypeInDBMigration _
      | DeleteColInDBMigration _
      | AbandonDBMigration _
      | DeleteDBCol _
      | RenameDBname _
      | CreateDBWithBlankOr _
      | DeleteTLForever _
      | DeleteFunctionForever _
      | SetType _
      | DeleteType _
      | DeleteTypeForever _ ->
          false )
    ops

let findOutUsages (tlid : tlid) (usages : usage list) : codeRef list =
  usages |> List.filterMap
    ~f:(fun (tlid_, otlid, mid) ->
      if tlid = tlid_
      then
        match mid with Some id -> Some (otlid, id) | None -> None
      else None
    )

let matchOutMeta (meta : tlMeta StrDict.t) (r : codeRef list) : tlReference list =
  r |> List.filterMap
    ~f:(fun (tlid_, id) ->
      let tlid = Prelude.showTLID tlid_ in
      (StrDict.get ~key:tlid meta)
      |> Option.andThen ~f:(fun m ->
        match m with
        | DBMeta (dBName, col) ->
          Some (OutReferenceDB (tlid_, dBName, col, id))
        | HandlerMeta (space, name, _) ->
          Some (OutReferenceEvent (tlid_, space, name, id))
        | FunctionMeta _ -> None
      )
    )

let findInUsages (tlid : tlid) (usages : usage list) : tlid list =
  usages |> List.filterMap
    ~f:(fun (intlid, outtlid, _) ->
      if outtlid = tlid
      then Some intlid
      else None
    )

let matchInMeta (meta : tlMeta StrDict.t) (r : tlid list) : tlReference list =
  r |> List.filterMap
    ~f:(fun tlid_ ->
      let tlid = Prelude.showTLID tlid_ in
      (StrDict.get ~key:tlid meta)
      |> Option.andThen ~f:(fun m ->
        match m with
        | HandlerMeta (space, name, modifier) ->
          Some (InReferenceHandler (tlid_, space, name, modifier))
        | FunctionMeta (name, params) ->
          Some (InReferenceFunction (tlid_, name, params))
        | _ -> None
      )
    )