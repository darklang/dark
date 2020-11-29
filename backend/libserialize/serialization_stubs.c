#include <string.h>
#include <stdio.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

extern char* dark_init_ocaml() {
  char* argv[2];
  argv[0] = "";
  argv[1] = NULL;
  printf("OCAML loaded!\n");
  value res = caml_startup_exn(argv);
  if (Is_exception_result(res)) {
    printf("OCAML exception !\n");
  }
  return strdup("loaded");
}
extern char* user_fn_of_binary_string_to_json (void* bytes) {
  static const value* closure = NULL;
  if (closure == NULL) closure = caml_named_value("user_fn_of_binary_string_to_json");
  value v = caml_copy_string(bytes);
  return strdup(String_val(caml_callback(*closure, v)));
}
extern char* user_tipe_of_binary_string_to_json (void* bytes) {
  // TODO: call the function via the OCAml runtime
}
extern char* handler_of_binary_string_to_json (void* bytes, int length) {
  static const value* closure = NULL;
  if (closure == NULL) closure = caml_named_value("handler_of_binary_string_to_json");
  value v = caml_alloc_initialized_string(length, bytes);
  value result = caml_callback(*closure, v);
  return strdup(String_val(result));
}
extern char* db_of_binary_string_to_json (void* bytes) {
  // TODO: call the function via the OCAml runtime
}
extern char* oplist_of_binary_string_to_json (void* bytes) {
  // TODO: call the function via the OCAml runtime
}
extern char* pos_of_binary_string_to_json (void* bytes) {
  // TODO: call the function via the OCAml runtime
}
