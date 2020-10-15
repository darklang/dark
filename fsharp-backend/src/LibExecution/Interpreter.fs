module LibExecution.Interpreter

open Thoth.Json.Net
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Runtime

// fsharplint:disable FL0039
let rec eval (state: ExecutionState) (st: Symtable.T) (e: Expr): DvalTask =
  let tryFindFn desc = state.functions.TryFind(desc)
  match e with
  | EInt i -> Plain(DInt i)
  | EString s -> Plain(DStr s)
  | ELet (lhs, rhs, body) ->
      let rhs = eval state st rhs
      rhs.bind (fun rhs ->
        let st = st.Add(lhs, rhs)
        eval state st body)
  | EFnCall (desc, exprs) ->
      (match tryFindFn desc with
       | Some fn ->
           Task
             (task {
               let! args = Runtime.map_s (eval state st) exprs
               return! (callFn state fn (Seq.toList args)).toTask()
              })
       | None -> Plain(err (NotAFunction desc)))
  | EBinOp (arg1, desc, arg2) ->
      (match tryFindFn desc with
       | Some fn ->
           let t1 = eval state st arg1
           let t2 = eval state st arg2
           t1.bind2 t2 (fun arg1 arg2 -> callFn state fn [ arg1; arg2 ])
       | None -> Plain(err (NotAFunction desc)))
  | ELambda (parameters, body) ->
      Plain
        (DLambda
          ({ symtable = st
             parameters = parameters
             body = body }))
  | EVariable (name) -> Plain(Symtable.get st name)
  | EIf (cond, thenbody, elsebody) ->
      let cond = eval state st cond
      cond.bind (function
        | DBool (true) -> eval state st thenbody
        | DBool (false) -> eval state st elsebody
        | cond -> Task(task { return (err (CondWithNonBool cond)) }))


and callFn (state: ExecutionState) (fn: BuiltInFn) (args: List<Dval>): DvalTask =
  match List.tryFind (fun (dv: Dval) -> dv.isFake) args with
  | Some special -> Plain special
  | None ->
      try
        fn.fn (state, args)
      with
      | RuntimeException rte -> Plain(err rte)
      | FnCallException FnFunctionRemoved -> Plain(err (FunctionRemoved fn.name))
      | FnCallException FnWrongTypes -> Plain(err (FnCalledWithWrongTypes(fn.name, args, fn.parameters)))
      | FakeDvalException dval -> Plain(dval)

and eval_lambda (state: ExecutionState) (l: Runtime.LambdaBlock) (args: List<Dval>): DvalTask =
  (* If one of the args is fake value used as a marker, return it instead of
   * executing. This is the same behaviour as in fn calls. *)
  match List.tryFind (fun (dv: Dval) -> dv.isFake) args with
  | Some dv -> Plain(dv)
  | None ->
      (* One of the reasons to take a separate list of params and args is to
       * provide this error message here. We don't have this information in
       * other places, and the alternative is just to provide incompletes
       * with no context *)
      if List.length l.parameters <> List.length args then
        Plain(err (LambdaCalledWithWrongCount(args, l.parameters)))
      else
        // FSTODO
        // let bindings = List.zip_exn params args in
        // List.iter bindings ~f:(fun ((id, paramName), dv) ->
        //     state.trace ~on_execution_path:state.on_execution_path id dv) ;
        let newSymtable =
          List.zip l.parameters args
          |> Map
          |> Map.union l.symtable

        eval state newSymtable l.body
