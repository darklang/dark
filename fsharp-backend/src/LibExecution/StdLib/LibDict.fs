open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { prefix_names = ["Dict::singleton"]
    ; infix_names = []
    ; parameters = [par "key" TStr; par "value" TAny]
    ; return_type = TObj
    ; description =
        "Returns a new dictionary with a single entry `key`: `value`."
    ; func =
        InProcess
          (function
          | _, [DStr k; v] ->
              DObj (DvalMap.singleton (Unicode_string.to_string k) v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::size"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TInt
    ; description =
        "Returns the number of entries in `dict` (the number of key-value pairs)."
    ; func =
        InProcess
          (function
          | _, [DObj o] -> o |> DvalMap.size |> Dval.dint | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::keys"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TList
    ; description = "Returns `dict`'s keys in a list, in an arbitrary order."
    ; func =
        InProcess
          (function
          | _, [DObj o] ->
              o
              |> DvalMap.keys
              |> List.map ~f:(fun k -> Dval.dstr_of_string_exn k)
              |> fun l -> DList l
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::values"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TList
    ; description = "Returns `dict`'s values in a list, in an arbitrary order."
    ; func =
        InProcess
          (function
          | _, [DObj o] -> DList (DvalMap.values o) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::toList"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TList
    ; description =
        "Returns `dict`'s entries as a list of `[key, value]` lists, in an arbitrary order. This function is the opposite of `Dict::fromList`."
    ; func =
        InProcess
          (function
          | _, [DObj o] ->
              DvalMap.to_list o
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> Dval.to_list
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::fromListOverwritingDuplicates"]
    ; infix_names = []
    ; parameters = [par "entries" TList]
    ; return_type = TObj
    ; description =
        "Returns a new dict with `entries`. Each value in `entries` must be a `[key, value]` list, where `key` is a `String`.
        If `entries` contains duplicate `key`s, the last entry with that key will be used in the resulting dictionary (use `Dict::fromList` if you want to enforce unique keys).
        This function is the opposite of `Dict::toList`."
    ; func =
        InProcess
          (function
          | state, [DList l] ->
              let fold_fn
                  (idx : int)
                  (acc : (dval DvalMap.t, dval (* type error *)) result)
                  (dv : dval) : (dval DvalMap.t, dval (* type error *)) result =
                Result.bind acc ~f:(fun acc ->
                    match dv with
                    | DList [DStr k; value] ->
                        Ok
                          ( acc
                          |> DvalMap.insert
                               ~key:(Unicode_string.to_string k)
                               ~value )
                    | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                        Error dv
                    | v ->
                        let err_details =
                          match v with
                          | DList [k; _] ->
                              let tipe =
                                k
                                |> Dval.tipe_of
                                |> Dval.tipe_to_developer_repr_v0
                              in
                              Printf.sprintf
                                "Keys must be `String`s but the type of `%s` is `%s`."
                                (Dval.to_developer_repr_v0 k)
                                tipe
                          | DList l ->
                              Printf.sprintf
                                "It has length %i but must have length 2."
                                (List.length l)
                          | non_list ->
                              let tipe =
                                non_list
                                |> Dval.tipe_of
                                |> Dval.tipe_to_developer_repr_v0
                              in
                              Printf.sprintf
                                "It is of type `%s` instead of `List`."
                                tipe
                        in
                        Error
                          (DError
                             ( SourceNone
                             , Printf.sprintf
                                 "Expected every value within the `entries` argument passed to `%s` to be a `[key, value]` list. However, that is not the case for the value at index %i: `%s`. %s"
                                 state.executing_fnname
                                 idx
                                 (Dval.to_developer_repr_v0 v)
                                 err_details )))
              in
              let result =
                l |> List.foldi ~init:(Ok DvalMap.empty) ~f:fold_fn
              in
              (match result with Ok res -> DObj res | Error v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::fromList"]
    ; infix_names = []
    ; parameters = [par "entries" TList]
    ; return_type = TOption
    ; description =
        "Each value in `entries` must be a `[key, value]` list, where `key` is a `String`.
        If `entries` contains no duplicate keys, returns `Just dict` where `dict` has `entries`.
        Otherwise, returns `Nothing` (use `Dict::fromListOverwritingDuplicates` if you want to overwrite duplicate keys)."
    ; func =
        InProcess
          (function
          | state, [DList l] ->
              let fold_fn
                  (idx : int) (acc : (dval DvalMap.t, dval) result) (dv : dval)
                  : (dval DvalMap.t, dval) result =
                (* The dval for the result error could either be [Error DError] (in case of a type error)
                 * or an [Error (DOption OptNothing)] (in case there is a duplicate) *)
                Result.bind acc ~f:(fun acc ->
                    match dv with
                    | DList [DStr k; value] ->
                      ( match
                          DvalMap.insert_fail_override
                            ~key:(Unicode_string.to_string k)
                            ~value
                            acc
                        with
                      | `Ok dict ->
                          Ok dict
                      | `Duplicate ->
                          Error (DOption OptNothing) )
                    | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                        Error dv
                    | v ->
                        let err_details =
                          match v with
                          | DList [k; _] ->
                              let tipe =
                                k
                                |> Dval.tipe_of
                                |> Dval.tipe_to_developer_repr_v0
                              in
                              Printf.sprintf
                                "Keys must be `String`s but the type of `%s` `%s`."
                                (Dval.to_developer_repr_v0 k)
                                tipe
                          | DList l ->
                              Printf.sprintf
                                "It has length %i but must have length 2."
                                (List.length l)
                          | non_list ->
                              let tipe =
                                non_list
                                |> Dval.tipe_of
                                |> Dval.tipe_to_developer_repr_v0
                              in
                              Printf.sprintf
                                "It is of type `%s` instead of `List`."
                                tipe
                        in
                        Error
                          (DError
                             ( SourceNone
                             , Printf.sprintf
                                 "Expected every value within the `entries` argument passed to `%s` to be a `[key, value]` list. However, that is not the case for the value at index %i: `%s`. %s"
                                 state.executing_fnname
                                 idx
                                 (Dval.to_developer_repr_v0 v)
                                 err_details )))
              in
              let result =
                l
                |> List.foldi ~init:(Ok DvalMap.empty) ~f:fold_fn
                |> Result.map ~f:(fun o -> DOption (OptJust (DObj o)))
              in
              (match result with Ok res -> res | Error v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::get"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr]
    ; return_type = TAny
    ; description =
        "Looks up `key` in object `dict` and returns the value if found, and Error otherwise"
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o ~key:(Unicode_string.to_string s) with
            | Some d ->
                d
            | None ->
                DNull )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Dict::get_v1"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr]
    ; return_type = TOption
    ; description = "Looks up `key` in object `dict` and returns an option"
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o ~key:(Unicode_string.to_string s) with
            | Some d ->
                DOption (OptJust d)
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Dict::get_v2"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr]
    ; return_type = TOption
    ; description =
        "If the `dict` contains `key`, returns the corresponding value, wrapped in an option: `Just value`. Otherwise, returns `Nothing`."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o ~key:(Unicode_string.to_string s) with
            | Some d ->
                Dval.to_opt_just d
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::member"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr]
    ; return_type = TBool
    ; description =
        "Returns `true` if the `dict` contains an entry with `key`, and `false` otherwise."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
              let key = Unicode_string.to_string s in
              DBool (DvalMap.contains_key o ~key)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::foreach"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; func ["val"]]
    ; return_type = TObj
    ; description =
        "Returns a new dictionary that contains the same keys as the original `dict` with values that have been transformed by `f`, which operates on each value."
    ; func =
        InProcess
          (function
          | state, [DObj o; DBlock b] ->
              let f dv = Ast.execute_dblock ~state b [dv] in
              DObj (Map.map ~f o)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Dict::map"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; func ["key"; "value"]]
    ; return_type = TObj
    ; description =
        "Returns a new dictionary that contains the same keys as the original `dict` with values that have been transformed by `f`, which operates on each key-value pair.
        Consider `Dict::filterMap` if you also want to drop some of the entries."
    ; func =
        InProcess
          (function
          | state, [DObj o; DBlock b] ->
              let f ~key ~(data : dval) =
                Ast.execute_dblock ~state b [Dval.dstr_of_string_exn key; data]
              in
              DObj (Map.mapi ~f o)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::filter"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; func ["key"; "value"]]
    ; return_type = TObj
    ; description =
        "Calls `f` on every entry in `dict`, returning a dictionary of only those entries for which `f key value` returns `true`.
        Consider `Dict::filterMap` if you also want to transform the entries."
    ; func =
        InProcess
          (function
          | state, [DObj o; DBlock b] ->
              let incomplete = ref false in
              let f ~(key : string) ~(data : dval) : bool =
                let result =
                  Ast.execute_dblock ~state b [Dval.dstr_of_string_exn key; data]
                in
                match result with
                | DBool b ->
                    b
                | DIncomplete _ ->
                    incomplete := true ;
                    false
                | v ->
                    RT.error
                      "Expecting fn to return bool"
                      ~result:v
                      ~actual:data
              in
              if !incomplete
              then DIncomplete SourceNone (*TODO(ds) source info *)
              else DObj (Base.Map.filteri ~f o)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Dict::filter_v1"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; func ["key"; "value"]]
    ; return_type = TObj
    ; description =
        "Evaluates `f key value` on every entry in `dict`. Returns a new dictionary that contains only the entries of `dict` for which `f` returned `true`."
    ; func =
        InProcess
          (function
          | state, [DObj o; DBlock b] ->
              let filter_propagating_errors ~key ~data acc =
                match acc with
                | Error dv ->
                    Error dv
                | Ok m ->
                    let result =
                      Ast.execute_dblock
                        ~state
                        b
                        [Dval.dstr_of_string_exn key; data]
                    in
                    ( match result with
                    | DBool true ->
                        Ok (Base.Map.set m ~key ~data)
                    | DBool false ->
                        Ok m
                    | (DIncomplete _ as e) | (DError _ as e) ->
                        Error e
                    | other ->
                        RT.error
                          "Fn returned incorrect type"
                          ~expected:"bool"
                          ~actual:other )
              in
              let filtered_result =
                Base.Map.fold
                  o
                  ~init:(Ok DvalMap.empty)
                  ~f:filter_propagating_errors
              in
              (match filtered_result with Ok o -> DObj o | Error dv -> dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::filterMap"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; func ["key"; "value"]]
    ; return_type = TObj
    ; description =
        {|Calls `f` on every entry in `dict`, returning a new dictionary that drops some entries (filter) and transforms others (map).
        If `f key value` returns `Nothing`, does not add `key` or `value` to the new dictionary, dropping the entry.
        If `f key value` returns `Just newValue`, adds the entry `key`: `newValue` to the new dictionary.
        This function combines `Dict::filter` and `Dict::map`.|}
    ; func =
        InProcess
          (function
          | state, [DObj o; DBlock b] ->
              let abortReason = ref None in
              let f ~key ~(data : dval) : dval option =
                if !abortReason = None
                then (
                  match
                    Ast.execute_dblock
                      ~state
                      b
                      [Dval.dstr_of_string_exn key; data]
                  with
                  | DOption (OptJust o) ->
                      Some o
                  | DOption OptNothing ->
                      None
                  | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                      abortReason := Some dv ;
                      None
                  | v ->
                      abortReason :=
                        Some
                          (DError
                             ( SourceNone
                             , "Expected the argument `f` passed to `"
                               ^ state.executing_fnname
                               ^ "` to return `Just` or `Nothing` for every entry in `dict`. However, it returned `"
                               ^ Dval.to_developer_repr_v0 v
                               ^ "` for the entry `"
                               ^ key
                               ^ " : "
                               ^ Dval.to_developer_repr_v0 data
                               ^ "`." )) ;
                      None )
                else None
              in
              let result = Map.filter_mapi ~f o in
              (match !abortReason with None -> DObj result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::empty"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description = "Returns an empty dictionary."
    ; func =
        InProcess (function _, [] -> DObj DvalMap.empty | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::isEmpty"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TBool
    ; description = "Returns `true` if the `dict` contains no entries."
    ; func =
        InProcess
          (function
          | _, [DObj dict] -> DBool (DvalMap.is_empty dict) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::merge"]
    ; infix_names = []
    ; parameters = [par "left" TObj; par "right" TObj]
    ; return_type = TObj
    ; description =
        "Returns a combined dictionary with both dictionaries' entries. If the same key exists in both `left` and `right`, it will have the value from `right`."
    ; func =
        InProcess
          (function
          | _, [DObj l; DObj r] ->
              DObj (Stdlib_util.merge_right l r)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::toJSON"]
    ; infix_names = []
    ; parameters = [par "dict" TObj]
    ; return_type = TStr
    ; description = "Returns `dict` as a JSON string."
    ; func =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Dval.to_pretty_machine_json_v1
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::set"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr; par "val" TAny]
    ; return_type = TObj
    ; description = "Returns a copy of `dict` with the `key` set to `val`."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr k; v] ->
              DObj (Map.set o ~key:(Unicode_string.to_string k) ~data:v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Dict::remove"]
    ; infix_names = []
    ; parameters = [par "dict" TObj; par "key" TStr]
    ; return_type = TObj
    ; description =
        "If the `dict` contains `key`, returns a copy of `dict` with `key` and its associated value removed. Otherwise, returns `dict` unchanged."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr k] ->
              DObj (Map.remove o (Unicode_string.to_string k))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
