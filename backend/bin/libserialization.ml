open Libserialize

let () =
  Callback.register "handler_of_binary_string" Libserialize.Binary_serialization.handler_of_binary_string;
  Callback.register "oplist_of_binary_string" Libserialize.Binary_serialization.oplist_of_binary_string;
  ()