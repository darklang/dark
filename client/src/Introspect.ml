open Tc
open Types
open Prelude
module B = Blank
module TL = Toplevel
module TD = TLIDDict

let keyForHandlerSpec (space : string) (name : string) : string =
  space ^ ":" ^ name


let dbsByName (dbs : db TD.t) : tlid StrDict.t =
  dbs
  |> TD.filterMapValues ~f:(fun db ->
         db.dbName |> B.toMaybe |> Option.map ~f:(fun name -> (name, db.dbTLID))
     )
  |> StrDict.fromList


let handlersByName (hs : handler TD.t) : tlid StrDict.t =
  hs
  |> TD.mapValues ~f:(fun h ->
         let space =
           h.spec.space |> B.toMaybe |> Option.withDefault ~default:"_"
         in
         let name =
           h.spec.name |> B.toMaybe |> Option.withDefault ~default:"_"
         in
         let key = keyForHandlerSpec space name in
         (key, h.hTLID) )
  |> StrDict.fromList


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
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDDict.empty
  |> TLIDDict.tlids
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


let allIn (tlid : tlid) (m : model) : toplevel list =
  m.tlUsageBy
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDSet.empty
  |> TLIDSet.toList
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


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


let refreshUsages (m : model) (tlids : tlid list) : model =
  let databases = dbsByName m.dbs in
  let handlers = handlersByName m.handlers in
  let tlUsages, tlUsageBy =
    tlids
    |> List.map ~f:(fun tlid ->
           let tl = TL.getExn m tlid in
           getUsageFor tl databases handlers )
    |> List.concat
    |> List.foldl
         ~init:(m.tlUsages, m.tlUsageBy)
         ~f:(fun (usedTLID, usedByTLID, id) (usages, usageBy) ->
           let newUsages =
             let inner =
               TD.get ~tlid:usedTLID usages
               |> Option.withDefault ~default:TD.empty
             in
             let set =
               inner
               |> TD.get ~tlid:usedByTLID
               |> Option.withDefault ~default:TLIDSet.empty
               |> IDSet.add ~value:id
             in
             TD.insert
               ~tlid:usedTLID
               ~value:(TD.insert ~tlid:usedByTLID ~value:set inner)
               usages
           in
           let newUsageBy =
             TD.get ~tlid:usedByTLID usageBy
             |> Option.withDefault ~default:TLIDSet.empty
             |> TLIDSet.add ~value:usedTLID
             |> fun value -> TD.insert ~tlid:usedByTLID ~value usageBy
           in
           (newUsages, newUsageBy) )
  in
  {m with tlUsages; tlUsageBy}


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
