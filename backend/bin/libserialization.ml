open Libserialize

let () =
  let module BS = Binary_serialization in
  Callback.register
    "user_fn_of_binary_string_to_json"
    BS.user_fn_of_binary_string_to_json ;
  Callback.register
    "user_tipe_of_binary_string_to_json"
    BS.user_tipe_of_binary_string_to_json ;
  Callback.register
    "handler_of_binary_string_to_json"
    BS.handler_of_binary_string_to_json ;
  Callback.register "db_of_binary_string_to_json" BS.db_of_binary_string_to_json ;
  Callback.register
    "oplist_of_binary_string_to_json"
    BS.oplist_of_binary_string_to_json ;
  Callback.register
    "pos_of_binary_string_to_json"
    BS.pos_of_binary_string_to_json ;
  ()
