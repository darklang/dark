open Prelude
module B = BlankOr
module E = FluidExpression

type t = Types.functionsType [@@deriving show {with_path = false}]

type props = Types.functionsProps [@@deriving show {with_path = false}]

(* Returns the function named `name`. Returns Nothing if the function
  * can't be found - this shouldn't happen in theory but often does
  * in practice; for example, someone might delete a function and
  * then do a local undo. *)
let find (name : string) (functions : t) : function_ option =
  List.find functions.allowedFunctions ~f:(fun f -> f.fnName = name)


let empty : t =
  { builtinFunctions = []
  ; packageFunctions = TLIDDict.empty
  ; allowedFunctions = []
  ; previewUnsafeFunctions = StrSet.empty }


let globalRef = ref empty

let global () = !globalRef

(* Return a set containing names of previewUnsafe user functions.
 *
 * A userfunction is unsafe if it calls an unsafe function (and so is also
 * unsafe if any of its callees (the functions it calls) itself calls an unsafe
 * function. The functions that we know are unsafe are the builtin functions
 * that are marked as unsafe.
 *
 * You can think of this as a callgraph. The bottom of the callgraph are
 * functions that dont call any more functions (and thus are safe) or are
 * builtins (and thus have a flag to tell us if they are safe).
 *
 * So to figure out the complete set of unsafe userfunctions, we start at the
 * bottom of the callgraph, from the unsafe builtins. Then we go up the tree,
 * and mark any callers of those functions as unsafe. And that's the whole
 * algorithm. *)
let calculateUnsafeUserFunctions (props : props) (t : t) : StrSet.t =
  (* Construct a dependency tree (which is a reverse callgraph) so that we get
   * from a callee to a caller *)
  let dependencyTree =
    props.userFunctions
    |> TLIDDict.mapValues ~f:(fun uf ->
           uf.ufMetadata.ufmName
           |> B.toOption
           |> Option.map ~f:(fun caller ->
                  E.filterMap (FluidAST.toExpr uf.ufAST) ~f:(function
                      | EBinOp (_, callee, _, _, _) ->
                          Some (callee, caller)
                      | EFnCall (_, callee, _, _) ->
                          Some (callee, caller)
                      | _ ->
                          None))
           |> Option.withDefault ~default:[])
    |> List.concat
    |> List.foldl ~init:StrDict.empty ~f:(fun (callee, caller) dict ->
           StrDict.update ~key:callee dict ~f:(function
               | Some callers ->
                   Some (caller :: callers)
               | None ->
                   Some [caller]))
  in
  (* Get the initial set of unsafe functions *)
  (* TODO: what about unsafe packagemanager functions *)
  let unsafeBuiltins =
    t.builtinFunctions
    |> List.filterMap ~f:(fun f ->
           if f.fnPreviewSafety = Unsafe then Some f.fnName else None)
  in
  let worklist = ref unsafeBuiltins in
  (* The result set *)
  let unsafeFns = ref StrSet.empty in
  (* The worklist algorithm:
   *
   * Go through worklist of unsafe functions, starting with known-unsafe builtins:
   * - mark this function unsafe
   * - add the callers to the worklist
   *)
  while !worklist <> [] do
    match !worklist with
    | callee :: rest ->
        (* already processed *)
        if StrSet.has ~value:callee !unsafeFns
        then worklist := rest
        else (
          unsafeFns := StrSet.add ~value:callee !unsafeFns ;
          (* add callers to be processed *)
          let callers =
            StrDict.get ~key:callee dependencyTree
            |> Option.withDefault ~default:[]
          in
          worklist := rest @ callers )
    | _ ->
        ()
  done ;
  StrSet.removeMany ~values:unsafeBuiltins !unsafeFns


let testCalculateUnsafeUserFunctions = calculateUnsafeUserFunctions

let asFunctions (t : t) : function_ list = t.allowedFunctions

let builtins (t : t) : function_ list = t.builtinFunctions

let calculateAllowedFunctionsList (props : props) (t : t) : function_ list =
  (* We hide functions that are deprecated unless they are in use *)
  let filterAndSort (fns : function_ list) : function_ list =
    let isUsedOrIsNotDeprecated (f : function_) : bool =
      if f.fnDeprecated
      then
        StrDict.get props.usedFns ~key:f.fnName
        |> Option.withDefault ~default:0
        |> fun count -> count > 0
      else true
    in
    let fnNameWithoutVersion (f : function_) : string =
      f.fnName
      |> String.to_lower
      |> String.split ~on:"_v"
      |> List.getAt ~index:0
      |> Option.withDefault ~default:f.fnName
    in
    fns
    |> List.filter ~f:isUsedOrIsNotDeprecated
    |> List.sortBy ~f:(fun f ->
           (* don't call List.head here - if we have DB::getAll_v1 and
            * DB::getAll_v2, we want those to sort accordingly! *)
           f.fnName |> String.to_lower |> String.split ~on:"_v")
    |> List.groupWhile ~f:(fun f1 f2 ->
           fnNameWithoutVersion f1 = fnNameWithoutVersion f2)
    |> List.map ~f:List.reverse
    |> List.flatten
  in
  let userFunctionMetadata =
    props.userFunctions
    |> TLIDDict.mapValues ~f:(fun x -> x.ufMetadata)
    |> List.filterMap ~f:UserFunctions.ufmToF
    |> List.map ~f:(fun f ->
           { f with
             fnPreviewSafety =
               ( if StrSet.has t.previewUnsafeFunctions ~value:f.fnName
               then Unsafe
               else Safe ) })
  in
  let packageFunctions =
    t.packageFunctions
    |> TLIDDict.values
    |> List.map ~f:PackageManager.fn_of_packageFn
  in
  t.builtinFunctions @ userFunctionMetadata @ packageFunctions |> filterAndSort


let update (props : props) (t : t) : t =
  let allowedFunctions = calculateAllowedFunctionsList props t in
  let previewUnsafeFunctions = calculateUnsafeUserFunctions props t in
  let result = {t with allowedFunctions; previewUnsafeFunctions} in
  (* ugh *)
  globalRef := result ;
  result


let setBuiltins (builtins : function_ list) (props : props) (t : t) : t =
  {t with builtinFunctions = builtins} |> update props


let setPackages (fns : packageFns) (props : props) (t : t) : t =
  {t with packageFunctions = fns} |> update props
