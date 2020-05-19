open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

module Error : sig
  type 'expr_type t =
    (* Failed to find a TUserType (name, version) in our type environment.
     * Potentially a type was deleted and we didn't clean-up every use
     * (or a race condition! *)
    | TypeLookupFailure of string * int
    (* Type error in _values_, ie. expected a string but got an Int *)
    | TypeUnificationFailure of
        { expected_tipe : tipe
        ; actual_value : 'expr_type dval }
    (* Type error between a user record definition and the actual object
      * received, specifically in its keys. ie expected {a : *, b : * } but
      * got { a : *, b : *, c : *} -- note we currently expect an _exact_
      * match and do not support structural subtyping. *)
    | MismatchedRecordFields of
        { expected_fields : String.Set.t
        ; actual_fields : String.Set.t }

  val to_string : 'expr_type t -> string

  val list_to_string : 'expr_type t list -> string
end

val check_function_call :
     user_tipes:user_tipe list
  -> 'expr_type fn
  -> 'expr_type dval_map
  -> (unit, 'expr_type Error.t list) Result.t

val check_function_return_type :
     user_tipes:user_tipe list
  -> 'expr_type fn
  -> 'expr_type dval
  -> (unit, 'expr_type Error.t list) Result.t
