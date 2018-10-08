let getBy fn l = Belt.List.getBy l fn

type size = { width : int; height : int }
module Native = struct
  module Window = struct
    let window : Dom.window = [%bs.raw "window"]
    external getWidth : Dom.window -> int = "innerWidth" [@@bs.get]
    external getHeight : Dom.window -> int = "innerHeight" [@@bs.get]
    let size () : size =
      { width = getWidth window
      ; height = getHeight window
      }
  end

  module Random = struct
    let random () = Random.int 2147483647
  end
end

let (++) = (^)

module String = struct
  include String
  let toInt = int_of_string
  let toFloat = float_of_string
end

module Option = struct
  include Belt.Option
  let andThen (o: 'a option) (fn: 'a -> 'b option) : 'b option =
    match o with
    | None -> None
    | Some x -> fn x
end

module Result = struct
  include Belt.Result
  let withDefault = getWithDefault
end

module Regex = struct
  let regex s : Js.Re.t = Js.Re.fromString ("/" ^ s ^ "/")
  let contains re s = Js.Re.test s re
end

module List = struct
  include Belt.List
  let indexedMap fn l = Belt.List.mapWithIndex l fn
  let map2 fn a b = mapReverse2 a b fn |> reverse
end

module LE = struct
  let getAt i l = List.get l i
  let elemIndex a l =
    l
    |> Js.Array.from
    |> Js.Array.findIndex (fun b -> a = b)
end

module Char = struct
  include Char
  let toCode = code
  let fromCode = chr
end

module Tuple = struct
  let to_tuple2 a b = (a,b)

end

let reReplace re repl str =
  Js.String.replaceByRe (Js.Re.fromString ("/" ^ re ^ "/")) repl str


