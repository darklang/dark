open Tc
open FluidExpression
open Shared

let blank () : t = EBlank id

let str (str : string) : t = EString (id, str)

let int (int : int) : t = EInteger (id, string_of_int int)

let intStr (int : string) : t = EInteger (id, int)

let bool (b : bool) : t = EBool (id, b)

let float' (whole : int) (fraction : int) : t =
  EFloat (id, string_of_int whole, string_of_int fraction)


let floatStr (whole : string) (fraction : string) : t =
  EFloat (id, whole, fraction)


let null : t = ENull (gid ())

let record (rows : (string * t) list) : t =
  ERecord (id, List.map rows ~f:(fun (k, v) -> (k, v)))


let list (elems : t list) : t = EList (id, elems)

let pipeTarget = EPipeTarget (gid ())

let fn ?(ster = NoRail) (name : string) (args : t list) =
  EFnCall (id, name, args, ster)


let binop ?(ster = NoRail) (name : string) (arg0 : t) (arg1 : t)
    =
  EBinOp (id, name, arg0, arg1, ster)


let partial (str : string) (e : t) : t = EPartial (id, str, e)

let rightPartial (str : string) (e : t) : t =
  ERightPartial (id, str, e)


let leftPartial (str : string) (e : t) : t =
  ELeftPartial (id, str, e)


let var (name : string) : t = EVariable (id, name)

let fieldAccess (expr : t) (fieldName : string) : t =
  EFieldAccess (id, expr, fieldName)


let if' (cond : t) (then' : t) (else' : t) : t =
  EIf (id, cond, then', else')


let let' (varName : string) (rhs : t) (body : t) : t =
  ELet (id, varName, rhs, body)


let lambda (varNames : string list) (body : t) : t =
  ELambda (id, List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe (first : t) (rest : t list) : t =
  EPipe (id, first :: rest)


let constructor (name : string) (args : t list) : t =
  EConstructor (id, name, args)


let just (arg : t) : t = EConstructor (id, "Just", [arg])

let nothing () : t = EConstructor (id, "Nothing", [])

let error (arg : t) : t = EConstructor (id, "Error", [arg])

let ok (arg : t) : t = EConstructor (id, "Ok", [arg])

let match' (cond : t) (matches : (FluidPattern.t * t) list) : t =
  EMatch (id, cond, matches)


let pInt (int : int) : FluidPattern.t =
  FPInteger (mid, id, string_of_int int)


let pIntStr (int : string) : FluidPattern.t =
  FPInteger (mid, id, int)


let pVar (name : string) : FluidPattern.t =
  FPVariable (mid, id, name)


let pConstructor
    ?(mid = gid ())
    ?(id = gid ())
    (name : string)
    (patterns : FluidPattern.t list) : FluidPattern.t =
  FPConstructor (mid, id, name, patterns)


let pJust (arg : FluidPattern.t) : FluidPattern.t
    =
  FPConstructor (mid, id, "Just", [arg])


let pNothing () : FluidPattern.t =
  FPConstructor (mid, id, "Nothing", [])


let pError (arg : FluidPattern.t) :
    FluidPattern.t =
  FPConstructor (mid, id, "Error", [arg])


let pOk (arg : FluidPattern.t) : FluidPattern.t =
  FPConstructor (mid, id, "Ok", [arg])


let pBool (b : bool) : FluidPattern.t =
  FPBool (mid, id, b)


let pString (str : string) : FluidPattern.t =
  FPString {matchID = mid; patternID = id; str}


let pFloatStr
    (whole : string) (fraction : string) :
    FluidPattern.t =
  FPFloat (mid, id, whole, fraction)


let pFloat (whole : int) (fraction : int) :
    FluidPattern.t =
  FPFloat (mid, id, string_of_int whole, string_of_int fraction)


let pNull () : FluidPattern.t = FPNull (mid, id)

let pBlank () : FluidPattern.t = FPBlank (mid, id)

let flag ?(name = "flag-1") cond oldCode newCode =
  EFeatureFlag (id, name, cond, oldCode, newCode)
