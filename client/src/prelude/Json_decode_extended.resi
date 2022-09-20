/* This is designed to be used as Json.Decode in prelude. Don't use it
 * directly unless you have to. */

include module type of Json.Decode

let variant5: (
  ('b, 'c, 'd, 'e, 'f) => 'a,
  decoder<'b>,
  decoder<'c>,
  decoder<'d>,
  decoder<'e>,
  decoder<'f>,
) => decoder<'a>

let variant4: (
  ('b, 'c, 'd, 'e) => 'a,
  decoder<'b>,
  decoder<'c>,
  decoder<'d>,
  decoder<'e>,
) => decoder<'a>

let variant3: (('b, 'c, 'd) => 'a, decoder<'b>, decoder<'c>, decoder<'d>) => decoder<'a>

let variant2: (('b, 'c) => 'a, decoder<'b>, decoder<'c>) => decoder<'a>

let variant1: ('b => 'a, decoder<'b>) => decoder<'a>

let variant0: 'a => decoder<'a>

/* For variants with record types, provide a constructor and three fields with
 * the field types, and decode them. For example, for the type:
 * MPString { matchID: id; patternID : id; str : string }
 *
 * the decoder would be
 *
 * recordVariant3
 *    (fun matchID patternID str -> P.MPString {matchID; patternID; str})
 *    ("matchID", id)
 *    ("patternID", id)
 *    ("str", string)
 */
let recordVariant3: (
  ('b, 'c, 'd) => 'a,
  (string, decoder<'b>),
  (string, decoder<'c>),
  (string, decoder<'d>),
) => decoder<'a>

let variants: list<(string, decoder<'a>)> => decoder<'a>

let result: (decoder<'ok>, decoder<'error>) => decoder<result<'ok, 'error>>

let succeed: 'a => decoder<'a>

let index: (int, decoder<'a>) => decoder<'a>

let tryDecode2: (decoder<'a>, decoder<'a>) => decoder<'a>

let strDict: decoder<'a> => decoder<Tc.Map.String.t<'a>>

let strListDict: decoder<'a> => decoder<Tc.Map.String.t<'a>>

let beltStrDict: decoder<'a> => decoder<Belt.Map.String.t<'a>>

let strSet: decoder<Tc.Set.String.t>

let decodeString: (decoder<'a>, string) => Tc.Result.t<'a, string>

let int64: decoder<int64>
let uint64: decoder<UInt64.t>

let date: decoder<Js.Date.t>

let float': decoder<float>

let tuple5: (
  decoder<'a>,
  decoder<'b>,
  decoder<'c>,
  decoder<'d>,
  decoder<'e>,
) => decoder<('a, 'b, 'c, 'd, 'e)>

let base64EncodedBytes: decoder<Bytes.t>
