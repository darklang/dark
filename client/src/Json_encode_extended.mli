include module type of Json.Encode

val variant : string -> Js.Json.t list -> Js.Json.t

val tcStrSet : Tc.StrSet.t -> Js.Json.t
