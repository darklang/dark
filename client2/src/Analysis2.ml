open Porting

(* Recieve Analysis *)

type analysis_result =
  { liveValues : string
  ; availableVarnames : string list
  }

module ReceiveAnalysis = struct
  let decode =
    let open Tea.Json.Decoder in
    let decodeVars =
      map
      (fun vars -> vars)
      (list string)
    in
    let decodeDetail =
      map2
        (fun lv vars ->
          { liveValues = lv
          ; availableVarnames = vars
          }
        )
        (field "liveValues" string)
        (field "availableVarnames" decodeVars)
    in
    map (fun detail -> detail)
      (field "detail" decodeDetail)
  let listen ?(key="") tagger =
    Porting.registerGlobal "receiveAnalysis" key tagger decode
end

(* Request analysis *)

module RequestAnalysis = struct

  external send : (int list -> unit) = "requestAnalysis" [@@bs.val][@@bs.scope "window", "darkAnalysis"]

end