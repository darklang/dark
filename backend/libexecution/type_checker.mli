open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

module Error : sig
  type t =
    | TypeLookupFailure of string * int
    | TypeUnificationFailure of {expected_tipe : tipe; actual_value : dval}
    | MismatchedRecordFields of
        { expected_fields : String.Set.t
        ; actual_fields : String.Set.t }
    | InvalidDefinition of user_record_field list
end

val check_function_call :
  user_tipes:user_tipe list -> fn -> dval_map -> (unit, Error.t list) Result.t
