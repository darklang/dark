open Libserialize

let () =
  let module BS = Binary_serialization in
  Callback.register "user_fn_bin2json" BS.user_fn_bin2json ;
  Callback.register "user_tipe_bin2json" BS.user_tipe_bin2json ;
  Callback.register "handler_bin2json" BS.handler_bin2json ;
  Callback.register "db_bin2json" BS.db_bin2json ;
  Callback.register "oplist_bin2json" BS.oplist_bin2json ;
  Callback.register "pos_bin2json" BS.pos_bin2json ;
  Callback.register "user_fn_json2bin" BS.user_fn_json2bin ;
  Callback.register "user_tipe_json2bin" BS.user_tipe_json2bin ;
  Callback.register "handler_json2bin" BS.handler_json2bin ;
  Callback.register "db_json2bin" BS.db_json2bin ;
  Callback.register "oplist_json2bin" BS.oplist_json2bin ;
  Callback.register "pos_json2bin" BS.pos_json2bin ;
  Callback.register "digest" BS.digest;
  ()
