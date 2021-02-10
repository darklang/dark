let t_internal_functions () =
  Libbackend.Account.set_admin "test" true ;
  check_dval
    "We should get an error on failed validation"
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "Invalid username 'Name with space', must match /^[a-z][a-z0-9_]{2,20}$/")))
    (exec_ast
       (fn
          "DarkInternal::upsertUser_v1"
          [ str "Name with space"
          ; str "valid@email.com"
          ; str "accidentalusername" ])) ;
  ()


let t_old_functions_deprecated () =
  let counts = ref StrDict.empty in
  List.iter (Core.String.Map.to_alist !Libs.static_fns) ~f:(fun (name, fn) ->
      let key = Str.global_replace (Str.regexp "_v[0-9]+") "" name in
      if not fn.deprecated
      then
        counts :=
          StrDict.update !counts ~key ~f:(fun count ->
              count |> Option.withDefault ~default:0 |> ( + ) 1 |> Some) ;
      ()) ;
  StrDict.iter !counts ~f:(fun name count ->
      AT.check AT.int (name ^ " only has one undeprecated fn") 1 count) ;
  ()


let t_libhttpclient () =
  check_error_contains
    "HttpClient::get_v5 illegal urls"
    (exec_ast
       (fn
          "HttpClient::get_v5"
          [str "http://thenonexistingurlforsure.com"; record []; record []]))
    "Couldn't resolve host name" ;
  ()


(* This test doesn't bother to destructure + examine the contents of the dobj;
 * it's just intended to ensure that the thing runs and doesn't DError. Esp
 * since the contents of the DObj depend on the database's stats and those
 * aren't stable in the test env! We could do a spot check on the keys present,
 * but I don't think that's terribly useful right now, type guarantees are
 * enough. *)
let t_darkinternal_table_stats_works () =
  let ast = fn "DarkInternal::getAndLogTableSizes" [] in
  AT.check
    AT.bool
    "DarkInternal::table_stats works (no params)"
    true
    (match exec_ast ast with DObj _ -> true | _ -> false) ;
  ()
