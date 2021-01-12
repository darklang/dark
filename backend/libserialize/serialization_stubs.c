#include <string.h>
#include <stdio.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>

/* --------------------
 * Initialize
 * -------------------- */

// OCaml is not reentrant; we need to acquire a lock before we call OCaml
// functions from C. Each thread also needs to be registered.

// https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#s%3AC-multithreading

void lock() {
  caml_acquire_runtime_system();
}

void unlock() {
  caml_release_runtime_system();
}

void check_string(value v) {
  if (Is_exception_result(v)) {
    printf("Value is exception!\n");
    fflush(stdout);
    unlock();
    // FSTODO remove
    exit(1);
  }
  if (Tag_val(v) != String_tag) {
    printf("Value is expected to be a string but isn't!\n");
    unlock();
    fflush(stdout);
    // FSTODO remove
    exit(1);
  }
}

// Allocates memory of exactly the size of the bytes in the value, and copies the
// data into it. Returns the new memory.
char* copy_bytes_outside_runtime(value v) {
  check_string(v);
  int length = caml_string_length(v);
  void* dest = malloc(length);
  memcpy(dest, String_val(v), length);
  return dest;
}

// Allocates memory sized 1 byte larger than the string in the value, copies
// the data, and adds a NULL byte at the end. Returns the new memory.
char* copy_string_outside_runtime(value v) {
  check_string(v);
  // OCaml strings can have NULL bytes in them, so don't use strndup
  int length = caml_string_length(v);
  char* dest = malloc(length+1);
  memcpy(dest, String_val(v), length);
  dest[length] = '\0';
  return dest;
}

extern char* dark_init_ocaml() {
  char* argv[2];
  argv[0] = "";
  argv[1] = NULL;
  printf("OCAML loaded!\n");
  value res = caml_startup_exn(argv);
  if (Is_exception_result(res)) {
    printf("OCAML exception !\n");
  }
  printf("Registering main thread!\n");
  caml_c_thread_register();
  // The main thread holds the lock - we need to release it or other threads
  // calling lock() will hang.
  caml_release_runtime_system();
  return strdup("loaded");
}

void register_thread() {
  caml_c_thread_register();
}

/* --------------------
 * Convert from json to binary strings
 * //FSTODO check for memory leaks in these functions
 * -------------------- */
char* call_bin2json(const char* callback_name, void* bytes, int length) {
  lock();
  value v = caml_alloc_initialized_string(length, bytes);
  check_string(v);
  value* closure = caml_named_value(callback_name);
  if (Is_exception_result(*closure)) {
    printf("Closure is exception!\n");
  }
  value result = caml_callback_exn(*closure, v);
  if (Is_exception_result(result)) {
    printf("result is exception!\n");
  }
  char* retval = copy_string_outside_runtime(result);
  unlock();
  return retval;
}
extern char* user_fn_bin2json(void* bytes, int length) {
  return call_bin2json("user_fn_bin2json", bytes, length);
}
extern char* user_tipe_bin2json(void* bytes, int length) {
  return call_bin2json("user_tipe_bin2json", bytes, length);
}
extern char* handler_bin2json(void* bytes, int length) {
  return call_bin2json("handler_bin2json", bytes, length);
}
extern char* db_bin2json(void* bytes, int length) {
  return call_bin2json("db_bin2json", bytes, length);
}
extern char* oplist_bin2json(void* bytes, int length) {
  return call_bin2json("oplist_bin2json", bytes, length);
}
extern char* pos_bin2json(void* bytes, int length) {
  return call_bin2json("pos_bin2json", bytes, length);
}
extern char* expr_bin2json(void* bytes, int length) {
  return call_bin2json("expr_bin2json", bytes, length);
}
extern char* expr_tlid_pair_bin2json(void* bytes, int length) {
  return call_bin2json("expr_tlid_pair_bin2json", bytes, length);
}

/* --------------------
 * Convert from json to binary strings
 * //FSTODO check for memory leaks in these functions
 * -------------------- */

// out_bytes is an out parameter, it passes a pointer to a memory location. We allocate memory then write the location into which we insert
int call_json2bin(const char* callback_name, char* json, void** out_bytes) {
  lock();
  value* closure = caml_named_value(callback_name);
  if (Is_exception_result(*closure)) {
    printf("call_json2bin: Closure is exception!\n");
  }
  value v = caml_copy_string(json);
  check_string(v);

  value result = caml_callback_exn(*closure, v);
  int length = caml_string_length(result);
  *out_bytes = copy_bytes_outside_runtime(result);
  unlock();
  return length;
}
extern int user_fn_json2bin(char* json, void** out_bytes) {
  return call_json2bin("user_fn_json2bin", json, out_bytes);
}
extern int user_tipe_json2bin(char* json, void** out_bytes) {
  return call_json2bin("user_tipe_json2bin", json, out_bytes);
}
extern int handler_json2bin(char* json, void** out_bytes) {
  return call_json2bin("handler_json2bin", json, out_bytes);
}
extern int db_json2bin(char* json, void** out_bytes) {
  return call_json2bin("db_json2bin", json, out_bytes);
}
extern int oplist_json2bin(char* json, void** out_bytes) {
  return call_json2bin("oplist_json2bin", json, out_bytes);
}
extern int pos_json2bin(char* json, void** out_bytes) {
  return call_json2bin("pos_json2bin", json, out_bytes);
}
extern int expr_json2bin(char* json, void** out_bytes) {
  return call_json2bin("expr_json2bin", json, out_bytes);
}
extern int expr_tlid_pair_json2bin(char* json, void** out_bytes) {
  return call_json2bin("expr_tlid_pair_json2bin", json, out_bytes);
}



/* --------------------
 * OCaml values
 * -------------------- */
extern char* digest () {
  lock();
  value* digest_value = caml_named_value("digest");
  char* result = copy_string_outside_runtime(*digest_value);
  unlock();
  return result;
}