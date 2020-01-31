open Tc
open FluidExpression
open Shared

let str (str : string) : t = EString (gid (), str)

let int (int : int) : t = EInteger (gid (), string_of_int int)

let intStr (int : string) : t = EInteger (gid (), int)

let bool (b : bool) : t = EBool (gid (), b)

let float' (whole : string) (fraction : string) : t =
  EFloat (gid (), whole, fraction)


let null : t = ENull (gid ())

let record (rows : (string * t) list) : t =
  ERecord (gid (), List.map rows ~f:(fun (k, v) -> (k, v)))


let list (elems : t list) : t = EList (gid (), elems)

let pipeTarget = EPipeTarget (gid ())

let fn ?(ster = NoRail) (name : string) (args : t list) =
  EFnCall (gid (), name, args, ster)


let binop ?(ster = NoRail) (name : string) (arg0 : t) (arg1 : t) =
  EBinOp (gid (), name, arg0, arg1, ster)


let partial (str : string) (e : t) : t = EPartial (gid (), str, e)

let rightPartial (str : string) (e : t) : t = ERightPartial (gid (), str, e)

let var (name : string) : t = EVariable (gid (), name)

let fieldAccess (expr : t) (fieldName : string) : t =
  EFieldAccess (gid (), expr, fieldName)


let if' (cond : t) (then' : t) (else' : t) : t = EIf (gid (), cond, then', else')

let let' (varName : string) (rhs : t) (body : t) : t =
  ELet (gid (), varName, rhs, body)


let lambda (varNames : string list) (body : t) : t =
  ELambda (gid (), List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe (first : t) (rest : t list) : t = EPipe (gid (), first :: rest)

let constructor (name : string) (args : t list) : t =
  EConstructor (gid (), name, args)


let match' (cond : t) (matches : (FluidPattern.t * t) list) : t =
  EMatch (gid (), cond, matches)


let pInt (int : int) : FluidPattern.t =
  FPInteger (gid (), gid (), string_of_int int)


let pIntStr (int : string) : FluidPattern.t = FPInteger (gid (), gid (), int)

let pVar (name : string) : FluidPattern.t = FPVariable (gid (), gid (), name)

let pConstructor (name : string) (patterns : FluidPattern.t list) :
    FluidPattern.t =
  FPConstructor (gid (), gid (), name, patterns)


let pBool (b : bool) : FluidPattern.t = FPBool (gid (), gid (), b)

let pString (str : string) : FluidPattern.t =
  FPString {matchID = gid (); patternID = gid (); str}


let pFloat (whole : string) (fraction : string) : FluidPattern.t =
  FPFloat (gid (), gid (), whole, fraction)


let pNull : FluidPattern.t = FPNull (gid (), gid ())

let pBlank : FluidPattern.t = FPBlank (gid (), gid ())
