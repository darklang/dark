type t =
  < hash : string
  ; host : string
  ; hostname : string
  ; href : string
  ; origin : string
  ; password : string
  ; pathname : string
  ; port : string
  ; protocol : string
  ; search : string
  ; username : string >
  Js.t

external make_internal : string -> t = "URL" [@@bs.new]

let make s = try Some (make_internal s) with _ -> None
