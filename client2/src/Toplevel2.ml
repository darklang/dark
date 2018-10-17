open Tea
open Helpers

type pos = { x : int; y : int }
type tl_type =
	| Database
	| RequestHandler
	| EventHandler
	| UserFunction

type model =
	{ id : int
	; pos : pos
	; kind: tl_type
	; content: string
	}

type msg =
	| Select
[@@bs.deriving {accessors}]

let type2string ty =
	match ty with
	| Database -> "db"
	| RequestHandler -> "http"
	| EventHandler -> "event"
	| UserFunction -> "ufun"

let new_http_handler content =
	{	id = Helpers.random_int 999
	; pos =
			{ x = Helpers.random_int 400 
			; y = Helpers.random_int 300
			}
	; kind = RequestHandler
	; content = content
	}

let get_dimensions id =
(* 	let open Json.Decoder in *)
	let str_id = Helpers.int2string id in
	let e = Helpers.get_element str_id in
(* 	let r = Helpers.get_rect e in *)
	Helpers.get_width e, Helpers.get_height e
(* 	(field "offsetWidth" e), (field "offsetHeight" e) *)

let is_inside (x : int ) (y : int) (m : model) =
	let str_id = Helpers.int2string m.id in
	let e = Helpers.get_element str_id in
	let w = Helpers.get_width e |> truncate in
	let h = Helpers.get_height e |> truncate in
	m.pos.x <= x && (m.pos.x + w) > x && m.pos.y <= y && (m.pos.y + h) > y

let update m = function
	| Select -> Js.log (get_dimensions m.id); m

let move (m : model) (dx : int) (dy : int) =
	{ m with
		pos =
			{ x = m.pos.x + dx
			; y = m.pos.y + dy
			}
	}

let view m =
	let open Html in
	node "toplevel"
		[ id (Helpers.int2string m.id)
		;	class' (type2string m.kind)
		; styles
				[ "left", Helpers.int_px m.pos.x
				; "top", Helpers.int_px m.pos.y
				]
		; onClick Select
		]
		[ text m.content ]