open Core_kernel

type fluidName = string

type fnName = string

type sendToRail =
  | Rail
  | NoRail

type id = Types.id

(* match id, then the pattern id. We have a pattern id cause they can be
 * nested. *)
type fluidPattern =
  | FPVariable of id * id * fluidName
  | FPConstructor of id * id * fluidName * fluidPattern list
  (* TODO: support char *)
  (* Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support *)
  | FPInteger of id * id * string
  | FPBool of id * id * bool
  | FPString of id * id * string
  | FPFloat of id * id * string * string
  | FPNull of id * id
  | FPBlank of id * id

type fluidExpr =
  (* ints in Bucklescript only support 32 bit ints but we want 63 bit int
   * support *)
  | EInteger of id * string
  | EBool of id * bool
  | EString of id * string
  | EFloat of id * string * string
  | ENull of id
  | EBlank of id
  | ELet of id * fluidName * fluidExpr * fluidExpr
  | EIf of id * fluidExpr * fluidExpr * fluidExpr
  | EBinOp of id * fluidName * fluidExpr * fluidExpr * sendToRail
  (* the id in the varname list is the analysis ID, used to get a livevalue
   * from the analysis engine *)
  | ELambda of id * (id * fluidName) list * fluidExpr
  | EFieldAccess of id * fluidExpr * fluidName
  | EVariable of id * string
  | EFnCall of id * fluidName * fluidExpr list * sendToRail
  | EPartial of id * string * fluidExpr
  | ERightPartial of id * string * fluidExpr
  | EList of id * fluidExpr list
  (* The ID in the list is extra for the fieldname *)
  | ERecord of id * (fluidName * fluidExpr) list
  | EPipe of id * fluidExpr list
  (* Constructors include `Just`, `Nothing`, `Error`, `Ok`.  In practice the
   * expr list is currently always length 1 (for `Just`, `Error`, and `Ok`)
   * or length 0 (for `Nothing`).
   *)
  | EConstructor of id * fluidName * fluidExpr list
  | EMatch of id * fluidExpr * (fluidPattern * fluidExpr) list
  (* Placeholder that indicates the target of the Thread. May be movable at
   * some point *)
  | EPipeTarget of id
  (* EFeatureFlag: id, flagName, condExpr, caseAExpr, caseBExpr *)
  | EFeatureFlag of id * string * fluidExpr * fluidExpr * fluidExpr

let gid () = Util.create_id ()

let newB () = Types.Blank (gid ())

let b = newB ()

let str (str : string) : fluidExpr = EString (gid (), str)

let int (int : string) : fluidExpr = EInteger (gid (), int)

let bool (b : bool) : fluidExpr = EBool (gid (), b)

let float' (whole : string) (fraction : string) : fluidExpr =
  EFloat (gid (), whole, fraction)


let null : fluidExpr = ENull (gid ())

let record (rows : (string * fluidExpr) list) : fluidExpr =
  ERecord (gid (), List.map rows ~f:(fun (k, v) -> (k, v)))


let list (elems : fluidExpr list) : fluidExpr = EList (gid (), elems)

let pipeTarget = EPipeTarget (gid ())

let fn ?(ster = NoRail) (name : string) (args : fluidExpr list) =
  EFnCall (gid (), name, args, ster)


let binop ?(ster = NoRail) (name : string) (arg0 : fluidExpr) (arg1 : fluidExpr)
    =
  EBinOp (gid (), name, arg0, arg1, ster)


let partial (str : string) (e : fluidExpr) : fluidExpr =
  EPartial (gid (), str, e)


let rightPartial (str : string) (e : fluidExpr) : fluidExpr =
  ERightPartial (gid (), str, e)


let var (name : string) : fluidExpr = EVariable (gid (), name)

let fieldAccess (expr : fluidExpr) (fieldName : string) : fluidExpr =
  EFieldAccess (gid (), expr, fieldName)


let if' (cond : fluidExpr) (then' : fluidExpr) (else' : fluidExpr) : fluidExpr =
  EIf (gid (), cond, then', else')


let let' (varName : string) (rhs : fluidExpr) (body : fluidExpr) : fluidExpr =
  ELet (gid (), varName, rhs, body)


let lambda (varNames : string list) (body : fluidExpr) : fluidExpr =
  ELambda (gid (), List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe (first : fluidExpr) (rest : fluidExpr list) : fluidExpr =
  EPipe (gid (), first :: rest)


let constructor (name : string) (args : fluidExpr list) : fluidExpr =
  EConstructor (gid (), name, args)


let match' (cond : fluidExpr) (matches : (fluidPattern * fluidExpr) list) :
    fluidExpr =
  EMatch (gid (), cond, matches)


let pInt (int : string) : fluidPattern = FPInteger (gid (), gid (), int)

let pVar (name : string) : fluidPattern = FPVariable (gid (), gid (), name)

let pConstructor (name : string) (patterns : fluidPattern list) : fluidPattern =
  FPConstructor (gid (), gid (), name, patterns)


let pBool (b : bool) : fluidPattern = FPBool (gid (), gid (), b)

let pString (str : string) : fluidPattern = FPString (gid (), gid (), str)

let pFloat (whole : string) (fraction : string) : fluidPattern =
  FPFloat (gid (), gid (), whole, fraction)


let pNull : fluidPattern = FPNull (gid (), gid ())

let pBlank : fluidPattern = FPBlank (gid (), gid ())

let literalToString
    (v : [> `Bool of bool | `Int of string | `Null | `Float of string * string])
    : string =
  match v with
  | `Int i ->
      i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


let rec fromFluidPattern (p : fluidPattern) : Types.RuntimeT.pattern =
  match p with
  | FPVariable (_, id, var) ->
      Filled (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      Filled (id, PConstructor (name, List.map ~f:fromFluidPattern patterns))
  | FPInteger (_, id, i) ->
      Filled (id, PLiteral (literalToString (`Int i)))
  | FPBool (_, id, b) ->
      Filled (id, PLiteral (literalToString (`Bool b)))
  | FPString (_, id, str) ->
      Filled (id, PLiteral (literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      Filled (id, PLiteral (literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      Filled (id, PLiteral (literalToString `Null))
  | FPBlank (_, id) ->
      Blank id


and fromFluidExpr (expr : fluidExpr) : Types.RuntimeT.expr =
  let open Types in
  let open Types.RuntimeT in
  let rec fromFluidExpr ?(inPipe = false) expr =
    (* inPipe is whether it's the immediate child of a pipe. *)
    let r = fromFluidExpr ~inPipe:false in
    match expr with
    | EInteger (id, num) ->
        Filled (id, Value (literalToString (`Int num)))
    | EString (id, str) ->
        Filled (id, Value (literalToString (`String str)))
    | EFloat (id, whole, fraction) ->
        Filled (id, Value (literalToString (`Float (whole, fraction))))
    | EBool (id, b) ->
        Filled (id, Value (literalToString (`Bool b)))
    | ENull id ->
        Filled (id, Value (literalToString `Null))
    | EVariable (id, var) ->
        Filled (id, Variable var)
    | EFieldAccess (id, obj, "") ->
        Filled (id, FieldAccess (fromFluidExpr obj, Blank (gid ())))
    | EFieldAccess (id, obj, fieldname) ->
        Filled (id, FieldAccess (fromFluidExpr obj, Filled (gid (), fieldname)))
    | EFnCall (id, name, args, ster) ->
      ( match args with
      | EPipeTarget _ :: _ when not inPipe ->
          newB ()
      | EPipeTarget _ :: args when inPipe ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, List.map ~f:r args))
          else Filled (id, FnCall (name, List.map ~f:r args))
      | _nonPipeTarget :: _ when inPipe ->
          newB ()
      | args ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, List.map ~f:r args))
          else Filled (id, FnCall (name, List.map ~f:r args)) )
    | EBinOp (id, name, arg1, arg2, ster) ->
      ( match arg1 with
      | EPipeTarget _ when not inPipe ->
          newB ()
      | EPipeTarget _ when inPipe ->
          if ster = Rail
          then Filled (id, FnCallSendToRail (name, [fromFluidExpr arg2]))
          else Filled (id, FnCall (name, [fromFluidExpr arg2]))
      | _nonPipeTarget when inPipe ->
          newB ()
      | _ ->
          if ster = Rail
          then
            Filled
              ( id
              , FnCallSendToRail (name, [fromFluidExpr arg1; fromFluidExpr arg2])
              )
          else
            Filled (id, FnCall (name, [fromFluidExpr arg1; fromFluidExpr arg2]))
      )
    | ELambda (id, vars, body) ->
        Filled
          ( id
          , Lambda
              ( List.map vars ~f:(fun (vid, var) -> Types.Filled (vid, var))
              , fromFluidExpr body ) )
    | EBlank id ->
        Blank id
    | ELet (id, lhs, rhs, body) ->
        Filled
          (id, Let (Filled (gid (), lhs), fromFluidExpr rhs, fromFluidExpr body))
    | EIf (id, cond, thenExpr, elseExpr) ->
        Filled
          ( id
          , If
              ( fromFluidExpr cond
              , fromFluidExpr thenExpr
              , fromFluidExpr elseExpr ) )
    | EPartial (id, str, oldVal) ->
        Filled (id, FluidPartial (str, fromFluidExpr ~inPipe oldVal))
    | ERightPartial (id, str, oldVal) ->
        Filled (id, FluidRightPartial (str, fromFluidExpr ~inPipe oldVal))
    | EList (id, exprs) ->
        Filled (id, ListLiteral (List.map ~f:r exprs))
    | ERecord (id, pairs) ->
        Filled
          ( id
          , ObjectLiteral
              (List.map pairs ~f:(fun (k, v) ->
                   (Types.Filled (gid (), k), fromFluidExpr v))) )
    | EPipe (id, exprs) ->
      ( match exprs with
      | head :: tail ->
          Filled
            ( id
            , Thread (r head :: List.map ~f:(fromFluidExpr ~inPipe:true) tail)
            )
      | [] ->
          Blank id )
    | EConstructor (id, name, exprs) ->
        Filled (id, Constructor (Filled (gid (), name), List.map ~f:r exprs))
    | EMatch (id, mexpr, pairs) ->
        let pairs =
          List.map pairs ~f:(fun (p, e) ->
              (fromFluidPattern p, fromFluidExpr e))
        in
        Filled (id, Match (fromFluidExpr mexpr, pairs))
    | EPipeTarget _ ->
        newB ()
    | EFeatureFlag (id, name, cond, caseA, caseB) ->
        Filled
          ( id
          , FeatureFlag
              ( Filled (gid (), name)
              , fromFluidExpr cond
              , fromFluidExpr ~inPipe caseA
              , fromFluidExpr ~inPipe caseB ) )
  in
  fromFluidExpr expr
