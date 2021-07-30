open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

(* ------------------------- *)
(* Toplevel manipulation *)
(* ------------------------- *)
let name (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      "H: " ^ (h.spec.name |> B.toOption |> Option.unwrap ~default:"")
  | TLDB db ->
      "DB: " ^ (db.dbName |> B.toOption |> Option.unwrap ~default:"")
  | TLPmFunc f ->
      "Package Manager Func: " ^ f.fnname
  | TLFunc f ->
      "Func: "
      ^ (f.ufMetadata.ufmName |> B.toOption |> Option.unwrap ~default:"")
  | TLTipe t ->
      "Type: " ^ (t.utName |> B.toOption |> Option.unwrap ~default:"")


let sortkey (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      (h.spec.space |> B.toOption |> Option.unwrap ~default:"Undefined")
      ^ (h.spec.name |> B.toOption |> Option.unwrap ~default:"Undefined")
      ^ (h.spec.modifier |> B.toOption |> Option.unwrap ~default:"")
  | TLDB db ->
      db.dbName |> B.toOption |> Option.unwrap ~default:"Undefined"
  | TLPmFunc f ->
      f.fnname
  | TLFunc f ->
      f.ufMetadata.ufmName |> B.toOption |> Option.unwrap ~default:""
  | TLTipe t ->
      t.utName |> B.toOption |> Option.unwrap ~default:""


let id tl =
  match tl with
  | TLHandler h ->
      h.hTLID
  | TLDB db ->
      db.dbTLID
  | TLFunc f ->
      f.ufTLID
  | TLPmFunc f ->
      f.pfTLID
  | TLTipe t ->
      t.utTLID


let pos tl =
  match tl with
  | TLHandler h ->
      h.pos
  | TLDB db ->
      db.pos
  | TLPmFunc f ->
      recover "no pos in a func" ~debug:f.pfTLID {x = 0; y = 0}
  | TLFunc f ->
      recover "no pos in a func" ~debug:f.ufTLID {x = 0; y = 0}
  | TLTipe t ->
      recover "no pos in a tipe" ~debug:t.utTLID {x = 0; y = 0}


let remove (m : model) (tl : toplevel) : model =
  let m = {m with cursorState = Deselected; currentPage = Architecture} in
  match tl with
  | TLHandler h ->
      Handlers.remove m h
  | TLDB db ->
      DB.remove m db
  | TLFunc f ->
      UserFunctions.remove m f
  | TLTipe ut ->
      UserTypes.remove m ut
  | TLPmFunc _ ->
      (* Cannot remove a package manager function *)
      m


let fromList (tls : toplevel list) : toplevel TLIDDict.t =
  tls |> List.map ~f:(fun tl -> (id tl, tl)) |> TD.fromList


let move (tlid : TLID.t) (xOffset : int) (yOffset : int) (m : model) : model =
  let newPos p = {x = p.x + xOffset; y = p.y + yOffset} in
  { m with
    handlers =
      Map.updateIfPresent m.handlers ~key:tlid ~f:(fun (h : handler) ->
          {h with pos = newPos h.pos})
  ; dbs =
      Map.updateIfPresent m.dbs ~key:tlid ~f:(fun (db : db) ->
          {db with pos = newPos db.pos}) }


let ufToTL (uf : userFunction) : toplevel = TLFunc uf

let pmfToTL (pmf : packageFn) : toplevel = TLPmFunc pmf

let utToTL (ut : userTipe) : toplevel = TLTipe ut

let asUserFunction (tl : toplevel) : userFunction option =
  match tl with TLFunc f -> Some f | _ -> None


let asUserTipe (tl : toplevel) : userTipe option =
  match tl with TLTipe t -> Some t | _ -> None


let isUserFunction (tl : toplevel) : bool =
  match tl with TLFunc _ -> true | _ -> false


let isUserTipe (tl : toplevel) : bool =
  match tl with TLTipe _ -> true | _ -> false


let asHandler (tl : toplevel) : handler option =
  match tl with TLHandler h -> Some h | _ -> None


let asDB (tl : toplevel) : db option =
  match tl with TLDB h -> Some h | _ -> None


let isDB (tl : toplevel) : bool = match tl with TLDB _ -> true | _ -> false

let isHandler (tl : toplevel) : bool =
  match tl with TLHandler _ -> true | _ -> false


let handlers (tls : toplevel list) : handler list =
  List.filterMap ~f:asHandler tls


let dbs (tls : toplevel TD.t) : db list = tls |> Map.filterMapValues ~f:asDB

let spaceOfHandler (h : handler) : handlerSpace = SpecHeaders.spaceOf h.spec

let spaceOf (tl : toplevel) : handlerSpace option =
  tl |> asHandler |> Option.map ~f:spaceOfHandler


let isHTTPHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSHTTP)

let isCronHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSCron)

let isWorkerHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSWorker)


let isReplHandler (tl : toplevel) : bool = tl |> spaceOf |> ( = ) (Some HSRepl)

let isDeprecatedCustomHandler (tl : toplevel) : bool =
  tl |> spaceOf |> ( = ) (Some HSDeprecatedOther)


let toOp (tl : toplevel) : op list =
  match tl with
  | TLHandler h ->
      [SetHandler (h.hTLID, h.pos, h)]
  | TLFunc fn ->
      [SetFunction fn]
  | TLTipe t ->
      [SetType t]
  | TLPmFunc _ ->
      recover "Package Manager functions are not editable" ~debug:(id tl) []
  | TLDB _ ->
      recover "This isn't how datastore ops work" ~debug:(id tl) []


(* ------------------------- *)
(* Generic *)
(* ------------------------- *)
let blankOrData (tl : toplevel) : blankOrData list =
  match tl with
  | TLHandler h ->
      SpecHeaders.blankOrData h.spec
  | TLDB db ->
      DB.blankOrData db
  | TLPmFunc f ->
      PackageManager.blankOrData f
  | TLFunc f ->
      UserFunctions.blankOrData f
  | TLTipe t ->
      UserTypes.blankOrData t


let isValidBlankOrID (tl : toplevel) (id : ID.t) : bool =
  List.member ~value:id (tl |> blankOrData |> List.map ~f:P.toID)


(* ------------------------- *)
(* ASTs *)
(* ------------------------- *)

let getAST (tl : toplevel) : FluidAST.t option =
  match tl with
  | TLHandler h ->
      Some h.ast
  | TLFunc f ->
      Some f.ufAST
  | TLPmFunc fn ->
      Some (FluidAST.ofExpr fn.body)
  | _ ->
      None


let setAST (tl : toplevel) (newAST : FluidAST.t) : toplevel =
  match tl with
  | TLHandler h ->
      TLHandler {h with ast = newAST}
  | TLFunc uf ->
      TLFunc {uf with ufAST = newAST}
  | TLDB _ | TLTipe _ | TLPmFunc _ ->
      tl


let withAST (m : model) (tlid : TLID.t) (ast : FluidAST.t) : model =
  { m with
    handlers =
      Map.updateIfPresent m.handlers ~key:tlid ~f:(fun h -> {h with ast})
  ; userFunctions =
      Map.updateIfPresent m.userFunctions ~key:tlid ~f:(fun uf ->
          {uf with ufAST = ast}) }


(* Create the modification to set the AST in this toplevel. `ops` is optional
 * other ops to include in this modification. Does not change the model. *)
let setASTMod ?(ops = []) (tl : toplevel) (ast : FluidAST.t) : modification =
  match tl with
  | TLHandler h ->
      if h.ast = ast
      then NoChange
      else
        AddOps (ops @ [SetHandler (id tl, h.pos, {h with ast})], FocusNoChange)
  | TLFunc f ->
      if f.ufAST = ast
      then NoChange
      else AddOps (ops @ [SetFunction {f with ufAST = ast}], FocusNoChange)
  | TLPmFunc _ ->
      recover "cannot change ast in package manager" ~debug:tl NoChange
  | TLTipe _ ->
      recover "no ast in Tipes" ~debug:tl NoChange
  | TLDB _ ->
      recover "no ast in DBs" ~debug:tl NoChange


(** modifyASTMod is a combination of getAST and setASTMod. It fetches the AST
  * for [tl] and passes it to [f], which should return a modified version of the
  * AST. An AddOps modification is returned, which updates the AST accordingly. *)
let modifyASTMod (tl : toplevel) ~(f : FluidAST.t -> FluidAST.t) : modification
    =
  getAST tl
  |> Option.map ~f:(f >> setASTMod tl)
  |> Option.unwrap ~default:NoChange


let replace (p : blankOrData) (replacement : blankOrData) (tl : toplevel) :
    toplevel =
  let id = P.toID p in
  match replacement with
  | PEventName bo | PEventModifier bo | PEventSpace bo ->
    ( match asHandler tl with
    | Some h ->
        let newSpec = SpecHeaders.replace id bo h.spec in
        TLHandler {h with spec = newSpec}
    | _ ->
        recover "Changing handler metadata on non-handler" ~debug:replacement tl
    )
  | PDBName _ | PDBColType _ | PDBColName _ ->
      tl
  | PFnName _ | PFnReturnTipe _ | PParamName _ | PParamTipe _ ->
    ( match asUserFunction tl with
    | Some fn ->
        let newFn = UserFunctions.replaceMetadataField p replacement fn in
        TLFunc newFn
    | _ ->
        recover "Changing fn metadata on non-fn" ~debug:replacement tl )
  | PTypeName _ | PTypeFieldName _ | PTypeFieldTipe _ ->
    ( match asUserTipe tl with
    | Some tipe ->
        let newTL = UserTypes.replace p replacement tipe in
        TLTipe newTL
    | _ ->
        recover "Changing tipe metadata on non-tipe" ~debug:replacement tl )


let combine
    (handlers : handler TD.t)
    (dbs : db TD.t)
    (userFunctions : userFunction TD.t)
    (packageFn : packageFn TD.t)
    (userTipes : userTipe TD.t) : toplevel TD.t =
  Map.map ~f:(fun h -> TLHandler h) handlers
  |> Map.mergeLeft (Map.map ~f:(fun db -> TLDB db) dbs)
  |> Map.mergeLeft (Map.map ~f:ufToTL userFunctions)
  |> Map.mergeLeft (Map.map ~f:pmfToTL packageFn)
  |> Map.mergeLeft (Map.map ~f:utToTL userTipes)


let all (m : model) : toplevel TD.t =
  combine
    m.handlers
    m.dbs
    m.userFunctions
    m.functions.packageFunctions
    m.userTipes


let structural (m : model) : toplevel TD.t =
  Map.map ~f:(fun h -> TLHandler h) m.handlers
  |> Map.mergeLeft (Map.map ~f:(fun db -> TLDB db) m.dbs)


let get (m : model) (tlid : TLID.t) : toplevel option =
  Map.get ~key:tlid (all m)


let find (tl : toplevel) (id_ : ID.t) : blankOrData option =
  blankOrData tl
  |> List.filter ~f:(fun d -> id_ = P.toID d)
  |> assertFn
       "cant find pd for id"
       ~debug:(id tl, id)
       ~f:(fun r -> List.length r <= 1)
  (* guard against dups *)
  |> List.head


let getPD (m : model) (tlid : TLID.t) (id : ID.t) : blankOrData option =
  get m tlid |> Option.andThen ~f:(fun tl -> find tl id)


let getTLAndPD (m : model) (tlid : TLID.t) (id : ID.t) :
    (toplevel * blankOrData option) option =
  get m tlid |> Option.map ~f:(fun tl -> (tl, find tl id))


let allDBNames (dbs : db TD.t) : string list =
  dbs
  |> Map.filterMapValues ~f:(fun db ->
         match db.dbName with F (_, name) -> Some name | Blank _ -> None)


let allGloballyScopedVarnames (dbs : db TD.t) : string list = allDBNames dbs

let asPage (tl : toplevel) (center : bool) : page =
  match tl with
  | TLHandler _ ->
      FocusedHandler (id tl, None, center)
  | TLDB _ ->
      FocusedDB (id tl, center)
  | TLPmFunc _ | TLFunc _ ->
      FocusedFn (id tl, None)
  | TLTipe _ ->
      FocusedType (id tl)


let selected (m : model) : toplevel option =
  m.cursorState |> CursorState.tlidOf |> Option.andThen ~f:(get m)


let selectedAST (m : model) : FluidAST.t option =
  selected m |> Option.andThen ~f:getAST


let setSelectedAST (m : model) (ast : FluidAST.t) : modification =
  match selected m with None -> NoChange | Some tl -> setASTMod tl ast


(* ------------------------- *)
(* Blanks *)
(* ------------------------- *)

type predecessor = ID.t option

type successor = ID.t option

let allBlanks (tl : toplevel) : ID.t list =
  (tl |> blankOrData |> List.filter ~f:P.isBlank |> List.map ~f:P.toID)
  @ ( tl
    |> getAST
    |> Option.map ~f:FluidAST.blanks
    |> Option.unwrap ~default:[]
    |> List.map ~f:FluidExpression.toID )


let allIDs (tl : toplevel) : ID.t list =
  (tl |> blankOrData |> List.map ~f:P.toID)
  @ (tl |> getAST |> Option.map ~f:FluidAST.ids |> Option.unwrap ~default:[])


let firstBlank (tl : toplevel) : successor = tl |> allBlanks |> List.head

let lastBlank (tl : toplevel) : successor = tl |> allBlanks |> List.last

let getNextBlank (tl : toplevel) (id : ID.t) : successor =
  let all = allIDs tl in
  let index = List.elemIndex ~value:id all |> Option.unwrap ~default:(-1) in
  let blanks = allBlanks tl |> List.map ~f:ID.toString |> Set.String.fromList in
  all
  |> List.drop ~count:(index + 1)
  |> List.find ~f:(fun id -> Set.member blanks ~value:(ID.toString id))
  |> Option.orElse (firstBlank tl)


let getPrevBlank (tl : toplevel) (id : ID.t) : predecessor =
  let all = allIDs tl in
  let index =
    List.elemIndex ~value:id all |> Option.unwrap ~default:(List.length all)
  in
  let blanks = allBlanks tl |> List.map ~f:ID.toString |> Set.String.fromList in
  all
  |> List.take ~count:index
  |> List.reverse
  |> List.find ~f:(fun id -> Set.member blanks ~value:(ID.toString id))
  |> Option.orElse (lastBlank tl)
