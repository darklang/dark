module LibExecution.Interpreter

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open System.Threading.Tasks
open FSharp.Control.Tasks

open Runtime

// fsharplint:disable FL0039
let rec eval (env: Environment.T) (st: Symtable.T) (e: Expr): DvalTask =
  let tryFindFn desc = env.functions.TryFind(desc)
  match e with
  | EInt i -> Plain(DInt i)
  | EString s -> Plain(DStr s)
  | ELet (lhs, rhs, body) ->
      let rhs = eval env st rhs
      rhs.bind (fun rhs ->
        let st = st.Add(lhs, rhs)
        eval env st body)
  | EFnCall (desc, exprs) ->
      (match tryFindFn desc with
       | Some fn ->
           Task
             (task {
               let! args = Runtime.map_s exprs (eval env st)
               return! (callFn env fn (Seq.toList args)).toTask()
              })
       | None -> Plain(err (NotAFunction desc)))
  | EBinOp (arg1, desc, arg2) ->
      (match tryFindFn desc with
       | Some fn ->
           let t1 = eval env st arg1
           let t2 = eval env st arg2
           t1.bind2 t2 (fun arg1 arg2 -> callFn env fn [ arg1; arg2 ])
       | None -> Plain(err (NotAFunction desc)))
  | ELambda (vars, expr) -> Plain(DLambda(st, vars, expr))
  | EVariable (name) -> Plain(Symtable.get st name)
  | EIf (cond, thenbody, elsebody) ->
      let cond = eval env st cond
      cond.bind (function
        | DBool (true) -> eval env st thenbody
        | DBool (false) -> eval env st elsebody
        | cond -> Task(task { return (err (CondWithNonBool cond)) }))


and callFn (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>): DvalTask =
  match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
  | Some special -> Plain special
  | None ->
      match fn.fn (env, args) with
      | Ok result -> result
      | Error () -> Plain(err (FnCalledWithWrongTypes(fn.name, args, fn.parameters)))
