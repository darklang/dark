open Helpers

type size = { width : int; height : int }

module OnResize = struct
  let decode =
    let open Tea.Json.Decoder in
    let decodeDetail =
    	map2 (fun width height -> { width; height})
    	(field "width" int)
    	(field "height" int)
  	in
    map (fun msg -> msg)
      (field "detail" decodeDetail)
  let listen ?(key="") tagger =
    Helpers.registerGlobal "windowResize" key tagger decode
end

module OnFocusChange = struct
	let decode =
		let open Tea.Json.Decoder in
		map (fun visible -> visible)
			(field "detail" bool)
	let listen?(key="") tagger =
		Helpers.registerGlobal "windowFocusChange" key tagger decode
end