open Prelude

(* Returns the function named `name`. Returns Nothing if the function
  * can't be found - this shouldn't happen in theory but often does
  * in practice; for example, someone might delete a function and
  * then do a local undo. *)
let findByNameInList (name : string) (functions : function_ list) :
    function_ option =
  List.find functions ~f:(fun f -> f.fnName = name)


let updateFunctions (m : model) : model =
  (* We hide functions that are deprecated unless they are in use *)
  let filterAndSort (fns : Prelude.function_ list) : Prelude.function_ list =
    let isUsedOrIsNotDeprecated (f : Prelude.function_) : bool =
      (not f.fnDeprecated) || Refactor.usedFn m f.fnName
    in
    fns
    |> List.filter ~f:isUsedOrIsNotDeprecated
    |> List.sortBy ~f:(fun f ->
           (* don't call List.head here - if we have DB::getAll_v1 and
            * DB::getAll_v2, we want those to sort accordingly! *)
           f.fnName |> String.to_lower |> String.split ~on:"_v")
  in
  let userFunctionMetadata =
    m.userFunctions
    |> TLIDDict.mapValues ~f:(fun x -> x.ufMetadata)
    |> List.filterMap ~f:UserFunctions.ufmToF
    |> List.map ~f:(fun f ->
           { f with
             fnPreviewSafety =
               ( if StrSet.has m.previewUnsafeUserFunctions ~value:f.fnName
               then Unsafe
               else Safe ) })
  in
  let packageFunctions =
    m.packageFns
    |> TLIDDict.values
    |> List.map ~f:PackageManager.fn_of_packageFn
  in
  { m with
    functions =
      m.builtInFunctions @ userFunctionMetadata @ packageFunctions
      |> filterAndSort }
