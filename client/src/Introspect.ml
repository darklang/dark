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


let allRefersTo (tlid : tlid) (m : model) : toplevel list =
  m.tlRefersTo
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDSet.empty
  |> TLIDSet.toList
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


let allUsedIn (tlid : tlid) (m : model) : toplevel list =
  m.tlUsedIn
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDSet.empty
  |> TLIDSet.toList
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


let findUsagesInAST
    (tlid_ : tlid)
    (databases : tlid StrDict.t)
    (handlers : tlid StrDict.t)
    (functions : tlid StrDict.t)
    (ast : expr) : usage list =
  AST.allData ast
  |> List.filterMap ~f:(fun pd ->
         match pd with
         | PExpr (F (_, Variable name)) ->
             StrDict.get ~key:name databases
         | PExpr
             (F
               ( _
               , FnCall
                   ( F (_, "emit")
                   , [_; F (_, Value space_); F (_, Value name_)]
                   , _ ) )) ->
             let name = Util.removeQuotes name_ in
             let space = Util.removeQuotes space_ in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
         | PExpr (F (_, FnCall (F (_, "emit_v1"), [_; F (_, Value name_)], _)))
           ->
             let name = Util.removeQuotes name_ in
             let space = "WORKER" in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
         | PExpr (F (_, FnCall (F (_, name), _, _))) ->
             StrDict.get ~key:name functions
         | _ ->
             None )
  |> List.uniqueBy ~f:(fun (TLID str) -> str)
  |> List.map ~f:(fun t -> (tlid_, t))


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
  let tlRefersTo, tlUsedIn =
    tlids
    |> List.map ~f:(fun tlid ->
           let tl = TL.getExn m tlid in
           getUsageFor tl databases handlers functions )
    |> List.concat
    |> List.foldl
         ~init:(m.tlRefersTo, m.tlUsedIn)
         ~f:(fun (refersToTLID, usedInTLID) (refersTo, usedIn) ->
           let newRefersTo =
             TD.get ~tlid:refersToTLID refersTo
             |> Option.withDefault ~default:TLIDSet.empty
             |> TLIDSet.add ~value:usedInTLID
             |> fun value -> TD.insert ~tlid:refersToTLID ~value refersTo
           in
           let newUsedIn =
             TD.get ~tlid:usedInTLID usedIn
             |> Option.withDefault ~default:TLIDSet.empty
             |> TLIDSet.add ~value:refersToTLID
             |> fun value -> TD.insert ~tlid:usedInTLID ~value usedIn
           in
           (newRefersTo, newUsedIn) )
  in
  {m with tlRefersTo; tlUsedIn}


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
