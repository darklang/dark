open Tc
open FluidExpression
open Shared

let blank ?(id = gid ()) () : t = EBlank id

let str ?(id = gid ()) (str : string) : t = EString (id, str)

let int ?(id = gid ()) (int : int) : t = EInteger (id, string_of_int int)

let intStr ?(id = gid ()) (int : string) : t = EInteger (id, int)

let bool ?(id = gid ()) (b : bool) : t = EBool (id, b)

let float' ?(id = gid ()) (whole : int) (fraction : int) : t =
  EFloat (id, string_of_int whole, string_of_int fraction)


let floatStr ?(id = gid ()) (whole : string) (fraction : string) : t =
  EFloat (id, whole, fraction)


let null : t = ENull (gid ())

let record ?(id = gid ()) (rows : (string * t) list) : t =
  ERecord (id, List.map rows ~f:(fun (k, v) -> (k, v)))


let list ?(id = gid ()) (elems : t list) : t = EList (id, elems)

let pipeTarget = EPipeTarget (gid ())

let fn ?(id = gid ()) ?(ster = NoRail) (name : string) (args : t list) =
  EFnCall (id, name, args, ster)


let binop ?(id = gid ()) ?(ster = NoRail) (name : string) (arg0 : t) (arg1 : t)
    =
  EBinOp (id, name, arg0, arg1, ster)


let partial ?(id = gid ()) (str : string) (e : t) : t = EPartial (id, str, e)

let rightPartial ?(id = gid ()) (str : string) (e : t) : t =
  ERightPartial (id, str, e)


let leftPartial ?(id = gid ()) (str : string) (e : t) : t =
  ELeftPartial (id, str, e)


let var ?(id = gid ()) (name : string) : t = EVariable (id, name)

let fieldAccess ?(id = gid ()) (expr : t) (fieldName : string) : t =
  EFieldAccess (id, expr, fieldName)


let if' ?(id = gid ()) (cond : t) (then' : t) (else' : t) : t =
  EIf (id, cond, then', else')


let let' ?(id = gid ()) (varName : string) (rhs : t) (body : t) : t =
  ELet (id, varName, rhs, body)


let lambda ?(id = gid ()) (varNames : string list) (body : t) : t =
  ELambda (id, List.map varNames ~f:(fun name -> (gid (), name)), body)


let pipe ?(id = gid ()) (first : t) (rest : t list) : t =
  EPipe (id, first :: rest)


let constructor ?(id = gid ()) (name : string) (args : t list) : t =
  EConstructor (id, name, args)


let just ?(id = gid ()) (arg : t) : t = EConstructor (id, "Just", [arg])

let nothing ?(id = gid ()) () : t = EConstructor (id, "Nothing", [])

let error ?(id = gid ()) (arg : t) : t = EConstructor (id, "Error", [arg])

let ok ?(id = gid ()) (arg : t) : t = EConstructor (id, "Ok", [arg])

let match' ?(id = gid ()) (cond : t) (matches : (FluidPattern.t * t) list) : t =
  EMatch (id, cond, matches)


let pInt ?(mid = gid ()) ?(id = gid ()) (int : int) : FluidPattern.t =
  FPInteger (mid, id, string_of_int int)


let pIntStr ?(mid = gid ()) ?(id = gid ()) (int : string) : FluidPattern.t =
  FPInteger (mid, id, int)


let pVar ?(mid = gid ()) ?(id = gid ()) (name : string) : FluidPattern.t =
  FPVariable (mid, id, name)


let pConstructor
    ?(mid = gid ())
    ?(id = gid ())
    (name : string)
    (patterns : FluidPattern.t list) : FluidPattern.t =
  FPConstructor (mid, id, name, patterns)


let pJust ?(mid = gid ()) ?(id = gid ()) (arg : FluidPattern.t) : FluidPattern.t
    =
  FPConstructor (mid, id, "Just", [arg])


let pNothing ?(mid = gid ()) ?(id = gid ()) () : FluidPattern.t =
  FPConstructor (mid, id, "Nothing", [])


let pError ?(mid = gid ()) ?(id = gid ()) (arg : FluidPattern.t) :
    FluidPattern.t =
  FPConstructor (mid, id, "Error", [arg])


let pOk ?(mid = gid ()) ?(id = gid ()) (arg : FluidPattern.t) : FluidPattern.t =
  FPConstructor (mid, id, "Ok", [arg])


let pBool ?(mid = gid ()) ?(id = gid ()) (b : bool) : FluidPattern.t =
  FPBool (mid, id, b)


let pString ?(mid = gid ()) ?(id = gid ()) (str : string) : FluidPattern.t =
  FPString {matchID = mid; patternID = id; str}


let pFloatStr
    ?(mid = gid ()) ?(id = gid ()) (whole : string) (fraction : string) :
    FluidPattern.t =
  FPFloat (mid, id, whole, fraction)


let pFloat ?(mid = gid ()) ?(id = gid ()) (whole : int) (fraction : int) :
    FluidPattern.t =
  FPFloat (mid, id, string_of_int whole, string_of_int fraction)


let pNull ?(mid = gid ()) ?(id = gid ()) () : FluidPattern.t = FPNull (mid, id)

let pBlank ?(mid = gid ()) ?(id = gid ()) () : FluidPattern.t = FPBlank (mid, id)

let flag ?(id = gid ()) ?(name = "flag-1") cond oldCode newCode =
  EFeatureFlag (id, name, cond, oldCode, newCode)
