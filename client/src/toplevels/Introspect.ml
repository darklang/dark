open Prelude
module B = BlankOr
module TL = Toplevel
module TD = TLIDDict

let keyForHandlerSpec (space : string) (name : string) : string =
  space ^ ":" ^ name


let keyForTipe (name : string) (version : int) : string =
  name ^ ":" ^ Int.toString version


let dbsByName (dbs : db TD.t) : TLID.t StrDict.t =
  dbs
  |> TD.filterMapValues ~f:(fun db ->
         db.dbName
         |> B.toOption
         |> Option.map ~f:(fun name -> (name, db.dbTLID)))
  |> StrDict.fromList


let handlersByName (hs : handler TD.t) : TLID.t StrDict.t =
  hs
  |> TD.mapValues ~f:(fun h ->
         let space =
           h.spec.space |> B.toOption |> Option.withDefault ~default:"_"
         in
         let name =
           h.spec.name |> B.toOption |> Option.withDefault ~default:"_"
         in
         let key = keyForHandlerSpec space name in
         (key, h.hTLID))
  |> StrDict.fromList


let functionsByName (fns : userFunction TD.t) : TLID.t StrDict.t =
  fns
  |> TD.filterMapValues ~f:(fun fn ->
         fn.ufMetadata.ufmName
         |> B.toOption
         |> Option.map ~f:(fun name -> (name, fn.ufTLID)))
  |> StrDict.fromList


let packageFunctionsByName (fns : packageFn TD.t) : TLID.t StrDict.t =
  fns
  |> TD.mapValues ~f:(fun fn -> (fn |> PackageManager.extendedName, fn.pfTLID))
  |> StrDict.fromList


let tipesByName (uts : userTipe TD.t) : TLID.t StrDict.t =
  uts
  |> TD.mapValues ~f:(fun ut ->
         let name =
           ut.utName
           |> B.toOption
           (* Shouldn't happen: all tipes have a default name *)
           |> recoverOpt "tipes should have default names" ~default:"_"
         in
         let version = ut.utVersion in
         let key = keyForTipe name version in
         (key, ut.utTLID))
  |> StrDict.fromList


let tlidsToUpdateUsage (ops : op list) : TLID.t list =
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
             None)
  |> List.uniqueBy ~f:TLID.toString


let allRefersTo (tlid : TLID.t) (m : model) : (toplevel * ID.t list) list =
  m.tlRefersTo
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:IDPairSet.empty
  |> IDPairSet.toList
  |> List.foldl ~init:TLIDDict.empty ~f:(fun (tlid, id) dict ->
         ( TLIDDict.update ~tlid dict ~f:(function
               | None ->
                   Some (IDSet.fromList [id])
               | Some set ->
                   Some (IDSet.add ~value:id set))
           : IDSet.t TLIDDict.t ))
  |> TD.toList
  |> List.filterMap ~f:(fun (tlid, ids) ->
         TL.get m tlid |> Option.map ~f:(fun tl -> (tl, IDSet.toList ids)))


let allUsedIn (tlid : TLID.t) (m : model) : toplevel list =
  m.tlUsedIn
  |> TLIDDict.get ~tlid
  |> Option.withDefault ~default:TLIDSet.empty
  |> TLIDSet.toList
  |> List.filterMap ~f:(fun tlid -> TL.get m tlid)


let findUsagesInAST
    (tlid : TLID.t)
    ~(datastores : TLID.t StrDict.t)
    ~(handlers : TLID.t StrDict.t)
    ~(functions : TLID.t StrDict.t)
    ~(packageFunctions : TLID.t StrDict.t)
    (ast : FluidAST.t) : usage list =
  FluidAST.toExpr ast
  |> FluidExpression.filterMap ~f:(fun e ->
         match e with
         | EVariable (id, name) ->
             StrDict.get ~key:name datastores
             |> Option.map ~f:(fun dbTLID -> (dbTLID, id))
         | EFnCall (id, "emit", [_; EString (_, space_); EString (_, name_)], _)
           ->
             let name = Util.removeQuotes name_ in
             let space = Util.removeQuotes space_ in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
             |> Option.map ~f:(fun fnTLID -> (fnTLID, id))
         | EFnCall (id, "emit_v1", [_; EString (_, name_)], _) ->
             let name = Util.removeQuotes name_ in
             let space = "WORKER" in
             let key = keyForHandlerSpec space name in
             StrDict.get ~key handlers
             |> Option.map ~f:(fun fnTLID -> (fnTLID, id))
         | EFnCall (id, name, _, _) ->
             Option.orElse
               ( StrDict.get ~key:name functions
               |> Option.map ~f:(fun fnTLID -> (fnTLID, id)) )
               ( StrDict.get ~key:name packageFunctions
               |> Option.map ~f:(fun fnTLID -> (fnTLID, id)) )
         | _ ->
             None)
  |> List.map ~f:(fun (usedIn, id) -> {refersTo = tlid; usedIn; id})


let findUsagesInFunctionParams (tipes : TLID.t StrDict.t) (fn : userFunction) :
    usage list =
  (* Versions are slightly aspirational, and we don't have them in most of
   * the places we use tipes, including here *)
  let version = 0 in
  fn.ufMetadata.ufmParameters
  |> List.filterMap ~f:(fun p ->
         p.ufpTipe
         |> B.toOption
         |> Option.map ~f:Runtime.tipe2str
         |> Option.map ~f:(fun t -> keyForTipe t version)
         |> Option.andThen ~f:(fun key -> StrDict.get ~key tipes)
         |> Option.thenAlso ~f:(fun _ -> Some (B.toID p.ufpTipe)))
  |> List.map ~f:(fun (usedIn, id) -> {refersTo = fn.ufTLID; usedIn; id})


let getUsageFor
    (tl : toplevel)
    ~(datastores : TLID.t StrDict.t)
    ~(handlers : TLID.t StrDict.t)
    ~(functions : TLID.t StrDict.t)
    ~(packageFunctions : TLID.t StrDict.t)
    ~(tipes : TLID.t StrDict.t) : usage list =
  let astUsages =
    TL.getAST tl
    |> Option.map
         ~f:
           (findUsagesInAST
              (TL.id tl)
              ~datastores
              ~handlers
              ~functions
              ~packageFunctions)
    |> Option.withDefault ~default:[]
  in
  let fnUsages =
    TL.asUserFunction tl
    |> Option.map ~f:(findUsagesInFunctionParams tipes)
    |> Option.withDefault ~default:[]
  in
  (* TODO: tipes in other tipes *)
  astUsages @ fnUsages


let refreshUsages (m : model) (tlids : TLID.t list) : model =
  let datastores = dbsByName m.dbs in
  let handlers = handlersByName m.handlers in
  let functions = functionsByName m.userFunctions in
  let packageFunctions = packageFunctionsByName m.functions.packageFunctions in
  let tipes = tipesByName m.userTipes in
  (* We need to overwrite the already-stored results for the passed-in TLIDs.
   * So we clear tlRefers for these tlids, and remove them from the inner set
   * of tlUsedIn. *)
  let tlRefersToDict = TD.removeMany ~tlids m.tlRefersTo in
  let tlUsedInDict =
    TD.map m.tlUsedIn ~f:(fun tlidsReferedTo ->
        TLIDSet.removeMany tlidsReferedTo ~values:tlids)
  in
  let newTlUsedIn, newTlRefersTo =
    tlids
    |> List.filterMap ~f:(fun tlid ->
           let tl = TL.get m tlid in
           Option.map tl ~f:(fun tl ->
               getUsageFor
                 tl
                 ~datastores
                 ~handlers
                 ~functions
                 ~packageFunctions
                 ~tipes))
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
           (newUsedIn, newRefersTo))
  in
  {m with tlUsedIn = newTlUsedIn; tlRefersTo = newTlRefersTo}


let setHoveringReferences (tlid : TLID.t) (ids : ID.t list) : modification =
  let new_props x =
    match x with
    | None ->
        Some {Defaults.defaultHandlerProp with hoveringReferences = ids}
    | Some v ->
        Some {v with hoveringReferences = ids}
  in
  ReplaceAllModificationsWithThisOne
    (fun m ->
      ( {m with handlerProps = TLIDDict.update ~tlid ~f:new_props m.handlerProps}
      , Tea.Cmd.none ))
