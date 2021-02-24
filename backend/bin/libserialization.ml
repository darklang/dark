open Libserialize

let () =
  let module BS = Binary_serialization in
  Callback.register "user_fn_bin2json" BS.user_fn_bin2json ;
  Callback.register "user_tipe_bin2json" BS.user_tipe_bin2json ;
  Callback.register "handler_bin2json" BS.handler_bin2json ;
  Callback.register "db_bin2json" BS.db_bin2json ;
  Callback.register "oplist_bin2json" BS.oplist_bin2json ;
  Callback.register "pos_bin2json" BS.pos_bin2json ;
  Callback.register "expr_bin2json" BS.expr_bin2json ;
  Callback.register "expr_tlid_pair_bin2json" BS.expr_tlid_pair_bin2json ;
  Callback.register "user_fn_json2bin" BS.user_fn_json2bin ;
  Callback.register "user_tipe_json2bin" BS.user_tipe_json2bin ;
  Callback.register "handler_json2bin" BS.handler_json2bin ;
  Callback.register "db_json2bin" BS.db_json2bin ;
  Callback.register "oplist_json2bin" BS.oplist_json2bin ;
  Callback.register "pos_json2bin" BS.pos_json2bin ;
  Callback.register "expr_json2bin" BS.expr_json2bin ;
  Callback.register "expr_tlid_pair_json2bin" BS.expr_tlid_pair_json2bin ;
  Callback.register "digest" BS.digest ;
  Callback.register
    "of_internal_roundtrippable_v0"
    Fuzzing.of_internal_roundtrippable_v0 ;
  Callback.register
    "of_internal_roundtrippable_v0"
    Fuzzing.of_internal_roundtrippable_v0 ;
  Callback.register "to_internal_queryable_v0" Fuzzing.to_internal_queryable_v0 ;
  Callback.register "to_internal_queryable_v1" Fuzzing.to_internal_queryable_v1 ;
  Callback.register "to_developer_repr_v0" Fuzzing.to_developer_repr_v0 ;
  Callback.register
    "to_enduser_readable_text_v0"
    Fuzzing.to_enduser_readable_text_v0 ;
  Callback.register
    "to_pretty_machine_json_v1"
    Fuzzing.to_pretty_machine_json_v1 ;
  Callback.register "to_url_string" Fuzzing.to_url_string ;
  Callback.register "to_hashable_repr" Fuzzing.to_hashable_repr ;
  Callback.register "of_unknown_json_v1" Fuzzing.of_unknown_json_v1 ;
  Callback.register "hash_v0" Fuzzing.hash_v0 ;
  Callback.register "hash_v1" Fuzzing.hash_v1 ;
  ()
