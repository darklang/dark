open Tc
open Types
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


let functionsByName (fns : userFunction TD.t) : tlid StrDict.t =
  fns
  |> TD.filterMapValues ~f:(fun fn ->
         fn.ufMetadata.ufmName
         |> B.toMaybe
         |> Option.map ~f:(fun name -> (name, fn.ufTLID)) )
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


let allRefersTo (tlid : tlid) (m : model) : (toplevel * id list) list =
  m.tlRefersTo
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:IDPairSet.empty
  |> IDPairSet.toList
  |> List.foldl ~init:TLIDDict.empty ~f:(fun (tlid, id) dict ->
         ( TLIDDict.update ~tlid dict ~f:(function
               | None ->
                   Some (IDSet.fromList [id])
               | Some set ->
                   Some (IDSet.add ~value:id set) )
           : IDSet.t TLIDDict.t ) )
  |> TD.toList
  |> List.map ~f:(fun (tlid, ids) -> (TL.getExn m tlid, IDSet.toList ids))


let allUsedIn (tlid : tlid) (m : model) : toplevel list =
  m.tlUsedIn
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDSet.empty
  |> TLIDSet.toList
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


let findUsagesInAST
    (tlid : tlid)
    (databases : tlid StrDict.t)
    (handlers : tlid StrDict.t)
    (functions : tlid StrDict.t)
    (ast : expr) : usage list =
  AST.allData ast
  |> List.filterMap ~f:(fun pd ->
         match pd with
         | PExpr (F (id, Variable name)) ->
             StrDict.get ~key:name databases
             |> Option.map ~f:(fun dbTLID -> (dbTLID, id))
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
             |> Option.map ~f:(fun fnTLID -> (fnTLID, id))
         | PExpr
             (F (id, FnCall (F (_, "emit_v1"), [_; F (_, Value name_)], _))) ->
             let name = Util.removeQuotes name_ in
             let space = "WORKER" in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
             |> Option.map ~f:(fun fnTLID -> (fnTLID, id))
         | PExpr (F (id, FnCall (F (_, name), _, _))) ->
             StrDict.get ~key:name functions
             |> Option.map ~f:(fun fnTLID -> (fnTLID, id))
         | _ ->
             None )
  |> List.map ~f:(fun (usedIn, id) -> {refersTo = tlid; usedIn; id})


let getUsageFor
    (tl : toplevel)
    (databases : tlid StrDict.t)
    (handlers : tlid StrDict.t)
    (functions : tlid StrDict.t) : usage list =
  match tl with
  | TLHandler h ->
      findUsagesInAST h.hTLID databases handlers functions h.ast
  | TLDB _ ->
      []
  | TLFunc f ->
      findUsagesInAST f.ufTLID databases handlers functions f.ufAST
  | TLTipe _ ->
      []


let refreshUsages (m : model) (tlids : tlid list) : model =
  let databases = dbsByName m.dbs in
  let handlers = handlersByName m.handlers in
  let functions = functionsByName m.userFunctions in
  (* We need to overwrite the already-stored results for the passed-in TLIDs.
   * So we clear tlRefers for these tlids, and remove them from the inner set
   * of tlUsedIn.  *)
  let tlRefersToDict = TD.removeMany ~tlids m.tlRefersTo in
  let tlUsedInDict =
    TD.map m.tlUsedIn ~f:(fun tlidsReferedTo ->
        TLIDSet.removeMany tlidsReferedTo ~values:tlids )
  in
  let newTlUsedIn, newTlRefersTo =
    tlids
    |> List.map ~f:(fun tlid ->
           let tl = TL.getExn m tlid in
           getUsageFor tl databases handlers functions )
    |> List.concat
    |> List.foldl
         ~init:(tlUsedInDict, tlRefersToDict)
         ~f:(fun usage (usedIn, refersTo) ->
           let newRefersTo =
             TD.get ~tlid:usage.refersTo refersTo
             |> Option.withDefault ~default:TLIDSet.empty
             |> IDPairSet.add ~value:(usage.usedIn, usage.id)
             |> fun value -> TD.insert ~tlid:usage.refersTo ~value refersTo
           in
           let newUsedIn =
             TD.get ~tlid:usage.usedIn usedIn
             |> Option.withDefault ~default:TLIDSet.empty
             |> TLIDSet.add ~value:usage.refersTo
             |> fun value -> TD.insert ~tlid:usage.usedIn ~value usedIn
           in
           (newUsedIn, newRefersTo) )
  in
  {m with tlUsedIn = newTlUsedIn; tlRefersTo = newTlRefersTo}


let setHoveringReferences (tlid : tlid) (ids : id list) : modification =
  let new_props x =
    match x with
    | None ->
        Some {Defaults.defaultHandlerProp with hoveringReferences = ids}
    | Some v ->
        Some {v with hoveringReferences = ids}
  in
  TweakModel
    (fun m ->
      {m with handlerProps = TLIDDict.update ~tlid ~f:new_props m.handlerProps}
      )
