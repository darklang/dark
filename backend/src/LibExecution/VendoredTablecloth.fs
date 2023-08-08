/// A forked subset of Tablecloth
///
/// Kept separate from Tablecloth as, in LibExecution, we want to be very careful
/// about backward compatibility. Tablecloth is still very much in flux, so
/// we want to avoid using it in case something happens that breaks lots of things.
///
/// Our rule is to not use Tablecloth in LibExecution.
///
/// However, we do want things to be consistent, so we directly import
/// functions that we want to use here.
module LibExecution.VendoredTablecloth

module Map =
  let keys (map : Map<'k, 'v>) : List<'k> =
    seq {
      for KeyValue(key, _) in map do
        yield key
    }
    |> List.ofSeq

  let values (map : Map<'k, 'v>) : List<'v> =
    seq {
      for KeyValue(_, value) in map do
        yield value
    }
    |> List.ofSeq

  let all (f : 'v -> bool) (m : Map<'k, 'v>) : bool = Map.forall (fun _ v -> f v) m

  let map (f : 'a -> 'b) (m : Map<'k, 'a>) : Map<'k, 'b> = Map.map (fun _ v -> f v) m
  let mapWithIndex (f : 'k -> 'a -> 'b) (m : Map<'k, 'a>) : Map<'k, 'b> = Map.map f m

  let fold
    (initial : 'state)
    (f : 'state -> 'key -> 'value -> 'state)
    (map : Map<'key, 'value>)
    : 'state =
    Map.fold f initial map

  let get (k : 'k) (m : Map<'k, 'v>) : Option<'v> = Map.tryFind k m

module Tuple2 =
  let make (a : 'a) (b : 'b) : 'a * 'b = (a, b)
  let first (v1 : 'a, _ : 'b) : 'a = v1
  let second (_ : 'a, v2 : 'b) : 'b = v2
  let mapFirst (f : 'x -> 'r) (x : 'x, y : 'y) : 'r * 'y = (f x, y)
  let mapSecond (f : 'y -> 'r) (x : 'x, y : 'y) : 'x * 'r = (x, f y)


module String =
  let toLowercase (s : string) : string = s.ToLower()
  let toUppercase (s : string) : string = s.ToUpper()
  let startsWith (prefix : string) (s : string) : bool = s.StartsWith(prefix)
  let endsWith (suffix : string) (s : string) : bool = s.EndsWith(suffix)
  let includes (substring : string) (s : string) : bool = s.Contains(substring)

  let split (on : string) (s : string) : List<string> =
    // Splitting an empty string with `Split` produces `[""]`, which is unexpected
    if s = "" then [] else s.Split(on) |> List.ofArray

  let trim (s : string) : string = s.Trim()
  let dropLeft (count : int) (s : string) : string = s[count..]

  let dropRight (count : int) (s : string) : string =
    s[0 .. (s.Length - (count + 1))]

  let replace (old : string) (newStr : string) (s : string) : string =
    s.Replace(old, newStr)

  let toArray (s : string) : char array = s.ToCharArray()

module List =
  let filterMap (f : 'a -> Option<'b>) (xs : List<'a>) : List<'b> = List.choose f xs
  let any (f : 'a -> bool) (l : List<'a>) : bool = List.exists f l
  let all (f : 'a -> bool) (l : List<'a>) : bool = List.forall f l
  let find (f : 'a -> bool) (l : List<'a>) : 'a option = List.tryFind f l

  let fold (initial : 'b) (f : 'b -> 'a -> 'b) (l : List<'a>) : 'b =
    List.fold f initial l

  // VTTODO untested
  let foldi (initial : 'b) (f : int -> 'b -> 'a -> 'b) (l : List<'a>) : 'b =
    let rec loop i accum l =
      match l with
      | [] -> accum
      | x :: rest -> loop (i + 1) (f i accum x) rest

    loop 0 initial l


  let foldRight (initial : 'b) (f : 'b -> 'a -> 'b) (l : List<'a>) : 'b =
    List.foldBack (fun item accum -> f accum item) l initial

  let intersperse (sep : 'a) (l : List<'a>) : List<'a> =
    (match l with
     | [] -> []
     | [ x ] -> [ x ]
     | x :: rest -> x :: foldRight [] (fun acc x -> sep :: x :: acc) rest : 'a list)

module Result =
  let unwrapWith (f : 'err -> 'ok) (t : Result<'ok, 'err>) : 'ok =
    match t with
    | Ok v -> v
    | Error v -> f v

  [<CompilerMessageAttribute("Result.unwrapUnsafe is banned, use Prelude.Exception.unwrapResult* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let unwrapUnsafe = Tablecloth.Result.unwrapUnsafe

module Option =

  [<CompilerMessageAttribute("Option.unwrapUnsafe is banned, use Prelude.Exception.unwrapOption* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let unwrapUnsafe = Tablecloth.Option.unwrapUnsafe

  [<CompilerMessageAttribute("Option.get is banned, use Prelude.Exception.unwrapOption* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let get = Option.get

  let unwrap (def : 'a) (o : Option<'a>) : 'a =
    match o with
    | None -> def
    | Some value -> value
