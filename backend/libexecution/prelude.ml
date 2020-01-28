include UnsharedTypes

include (
  Tc :
    module type of Tc
      (* with module StrSet := Tc.StrSet *)
      (*  and module IntSet := Tc.IntSet *)
      (* with module StrDict := Tc.StrDict *)
      (* with module Option := Tc.Option *)
      (* with module String := Tc.String *)
      with module Result := Tc.Result
    (* and module List := Tc.List  *) )

module Result = struct
  include Tc.Result

  let ok_or_internal_exception (msg : string) (t : (string, 'a) t) : 'a =
    match t with
    | Ok a ->
        a
    | Error err ->
        Exception.internal ~info:[("error", err)] msg
end
