include Tc
include Types
include Recover

type jsonType = Js.Json.t

let reportError = ErrorReporting.reportError

/* Every other module should have `open Prelude` as its first statement.
 * You don't need to open/include Tc or Types, Prelude includes them. */

module Tea = {
  // Extend Tea functions
  module Result = Tea_result
  module Cmd = Tea_cmd
  module Sub = Tea_sub
  module App = Tea_app
  module Debug = Tea_debug
  module Html = Tea_html_extended
  module Html2 = Tea_html2
  module Svg = Tea_svg
  module Task = Tea_task
  module Program = Tea_program
  module Time = Tea_time_extended
  module Json = Tea_json
  module Navigation = Tea_navigation
  module Random = Tea_random
  module AnimationFrame = Tea_animationframe
  module Mouse = Tea_mouse
  module Http = Tea_http
  module Ex = Tea_ex
}

module Html = Tea.Html

module Json = {
  exception ParseError = Json.ParseError

  let parseOrRaise = Json.parseOrRaise

  let parse = Json.parse

  let stringify = Json.stringify

  module Decode = Json_decode_extended
  module Encode = Json_encode_extended
}

// ---
// IDs
// ---

let gid = () => Js_math.random_int(0, 2147483647) |> string_of_int |> ID.fromString

let gtlid = (): TLID.t => Util.random() |> string_of_int |> TLID.fromString

// --------------------------------------
// CursorState
// --------------------------------------

module Debug = {
  let log = (~f: 'a => 'b=x => Obj.magic(x), msg: string, data: 'a): 'a => {
    Js.log2(msg, f(data))
    data
  }

  let loG = (~f: 'a => 'b=x => Obj.magic(x), msg: string, data: 'a): unit => Js.log2(msg, f(data))
}

// Needs a better home

let rec tipe2str = (t: tipe): string =>
  switch t {
  | TAny => "Any"
  | TInt => "Int"
  | TFloat => "Float"
  | TBool => "Bool"
  | TNull => "Null"
  | TCharacter => "Character"
  | TStr => "String"
  | TList => "List"
  | TObj => "Dict"
  | TBlock => "Block"
  | TIncomplete => "Incomplete"
  | TError => "Error"
  | TResp => "Response"
  | TDB => "Datastore"
  | TDate => "Date"
  | TOption => "Option"
  | TPassword => "Password"
  | TUuid => "UUID"
  | TErrorRail => "ErrorRail"
  | TResult => "Result"
  | TDbList(a) => "[" ++ (tipe2str(a) ++ "]")
  | TUserType(name, _) => name
  | TBytes => "Bytes"
  }
