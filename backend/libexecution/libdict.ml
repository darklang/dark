open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { prefix_names = ["Dict::keys"]
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
        "Calls `f` on every entry in `dict`, returning a new dictionary that drops some entries (filter) and transforms others (map).
      If `f key value` returns `Nothing`, drops `key` and its value from the dictionary.
      If `f key value` returns `Just newValue`, sets `key` to `newValue` in the dictionary.
      This function combines `Dict::filter` and `Dict::map`."
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
              DObj (Util.merge_right l r)
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
