open Tc
open Types
open Prelude
module B = Blank
module TL = Toplevel

let keyForHandlerSpec (space : string) (name : string) : string =
  space ^ ":" ^ name


let dbsByName (toplevels : toplevel list) : tlid StrDict.t =
  List.foldl
    ~f:(fun tl res ->
      match tl with
      | TLDB db ->
        ( match db.dbName with
        | F (_, name) ->
            StrDict.insert ~key:name ~value:db.dbTLID res
        | Blank _ ->
            res )
      | _ ->
          res )
    ~init:StrDict.empty
    toplevels


let handlersByName (toplevels : toplevel list) : tlid StrDict.t =
  List.foldl
    ~f:(fun tl res ->
      match tl with
      | TLHandler h ->
          let space =
            h.spec.space |> B.toMaybe |> Option.withDefault ~default:"_"
          in
          let name =
            h.spec.name |> B.toMaybe |> Option.withDefault ~default:"_"
          in
          let name = keyForHandlerSpec space name in
          StrDict.insert ~key:name ~value:h.hTLID res
      | _ ->
          res )
    ~init:StrDict.empty
    toplevels


let tlidsToUpdateMeta (ops : op list) : tlid list =
  ops
  |> List.filterMap ~f:(fun op ->
         match op with
         | SetHandler (tlid, _, _)
         | AddDBCol (tlid, _, _)
         | SetDBColName (tlid, _, _)
         | SetDBColType (tlid, _, _)
         | DeleteDBCol (tlid, _)
         | RenameDBname (tlid, _) ->
             Some tlid
         | SetFunction f ->
             Some f.ufTLID
         | SetExpr _
         | CreateDB _
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
         | CreateDBWithBlankOr _
         | DeleteTLForever _
         | DeleteFunctionForever _
         | SetType _
         | DeleteType _
         | DeleteTypeForever _ ->
             None )
  |> List.uniqueBy ~f:(fun (TLID tlid) -> tlid)


let tlidsToUpdateUsage (ops : op list) : tlid list =
  ops
  |> List.filterMap ~f:(fun op ->
         match op with
         | SetHandler (tlid, _, _) | SetExpr (tlid, _, _) ->
             Some tlid
         | SetFunction f ->
             Some f.ufTLID
         | CreateDB _
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
         | CreateDBWithBlankOr _
         | DeleteTLForever _
         | DeleteFunctionForever _
         | SetType _
         | DeleteType _
         | AddDBCol _
         | DeleteTypeForever _
         | SetDBColType _
         | DeleteDBCol _
         | RenameDBname _
         | SetDBColName _ ->
             None )
  |> List.uniqueBy ~f:(fun (TLID tlid) -> tlid)


let allTo (tlid : tlid) (m : model) : toplevel list =
  m.tlUsages
  (* Filter for all outgoing references in given toplevel *)
  |> List.filter ~f:(fun (intlid, _, _) -> tlid = intlid)
  (* Match all outgoing references with their relevant display meta data *)
  |> List.filterMap ~f:(fun (_, outtlid, _) -> TL.get m outtlid)


let allIn (tlid : tlid) (m : model) : toplevel list =
  m.tlUsages
  (* Filter for all places where given tl is used  *)
  |> List.filter ~f:(fun (_, outtlid, _) -> outtlid = tlid)
  (* Match all used in references with their relevant display meta data *)
  |> List.filterMap ~f:(fun (intlid, _, _) -> TL.get m intlid)


let presentAvatars (m : model) : avatar list =
  let avatars = m.avatarsList in
  avatars


let replaceUsages (oldUsages : usage list) (newUsages : usage list) :
    usage list =
  let tlids =
    newUsages
    |> List.uniqueBy ~f:(fun (TLID tlid, _, _) -> tlid)
    |> List.map ~f:(fun (tlid, _, _) -> tlid)
  in
  let usagesToKeep =
    oldUsages
    |> List.filter ~f:(fun (tlid, _, _) -> not (List.member ~value:tlid tlids))
  in
  usagesToKeep @ newUsages


let findUsagesInAST
    (tlid_ : tlid)
    (databases : tlid StrDict.t)
    (handlers : tlid StrDict.t)
    (ast : expr) : usage list =
  AST.allData ast
  |> List.filterMap ~f:(fun pd ->
         match pd with
         | PExpr (F (id, Variable name)) ->
             StrDict.get ~key:name databases
             |> Option.andThen ~f:(fun tlid -> Some (tlid_, tlid, id))
         | PExpr
             (F
               ( id
               , FnCall
                   ( F (_, "emit")
                   , [_; F (_, Value space_); F (_, Value name_)]
                   , _ ) )) ->
             let name = Util.removeQuotes name_ in
             let space = Util.removeQuotes space_ in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
             |> Option.andThen ~f:(fun tlid -> Some (tlid_, tlid, id))
         | PExpr
             (F (id, FnCall (F (_, "emit_v1"), [_; F (_, Value name_)], _))) ->
             let name = Util.removeQuotes name_ in
             let space = "WORKER" in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
             |> Option.andThen ~f:(fun tlid -> Some (tlid_, tlid, id))
         | _ ->
             None )
  |> List.uniqueBy ~f:(fun (_, TLID tlid, _) -> tlid)


let getUsageFor
    (tl : toplevel) (databases : tlid StrDict.t) (handlers : tlid StrDict.t) :
    usage list =
  match tl with
  | TLHandler h ->
      findUsagesInAST h.hTLID databases handlers h.ast
  | TLDB _ ->
      []
  | TLFunc f ->
      findUsagesInAST f.ufTLID databases handlers f.ufAST
  | TLTipe _ ->
      []


let usageMod (ops : op list) (toplevels : toplevel list) : modification =
  let tlidsToUpdate = tlidsToUpdateUsage ops in
  let databases = dbsByName toplevels in
  let handlers = handlersByName toplevels in
  let use =
    toplevels
    |> List.filterMap ~f:(fun tl ->
           if List.member ~value:(TL.id tl) tlidsToUpdate
           then Some tl
           else None )
    |> List.foldl
         ~f:(fun tl refs -> refs @ getUsageFor tl databases handlers)
         ~init:[]
  in
  if List.isEmpty use then NoChange else UpdateTLUsage use


let initUsages (tls : toplevel list) : usage list =
  let databases = dbsByName tls in
  let handlers = handlersByName tls in
  List.foldl
    ~f:(fun tl refs -> refs @ getUsageFor tl databases handlers)
    ~init:[]
    tls


let setHoveringVarName (tlid : tlid) (name : varName option) : modification =
  let new_props x =
    match x with
    | None ->
        Some {Defaults.defaultHandlerProp with hoveringVariableName = name}
    | Some v ->
        Some {v with hoveringVariableName = name}
  in
  TweakModel
    (fun m ->
      { m with
        handlerProps =
          StrDict.update ~key:(showTLID tlid) ~f:new_props m.handlerProps } )
