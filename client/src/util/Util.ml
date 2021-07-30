open Tc

let random (a : unit) : int = Native.Random.random a

let findIndex ~(f : 'a -> bool) (l : 'a list) : (int * 'a) option =
  List.find ~f:(fun (_, a) -> f a) (List.mapWithIndex ~f:Tuple2.make l)


let listPrevious ~(value : 'a) (l : 'a list) : 'a option =
  l
  |> List.elemIndex ~value
  |> Option.map ~f:(fun x -> x - 1)
  |> Option.andThen ~f:(fun (index : int) -> List.getAt ~index l)


let listNext ~(value : 'a) (l : 'a list) : 'a option =
  l
  |> List.elemIndex ~value
  |> Option.map ~f:(fun x -> x + 1)
  |> Option.andThen ~f:(fun (index : int) -> List.getAt ~index l)


let listNextWrap ~(value : 'a) (l : 'a list) : 'a option =
  l |> listNext ~value |> Option.orElse (List.head l)


let removeQuotes (s : string) : string =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then s |> String.dropLeft ~count:1 |> String.dropRight ~count:1
  else s


let humanReadableTimeElapsed (time : float) : string =
  let msPerMinute = 60.0 *. 1000.0 in
  let msPerHour = msPerMinute *. 60.0 in
  let msPerDay = msPerHour *. 24.0 in
  let msPerMonth = msPerDay *. 30.0 in
  let msPerYear = msPerDay *. 365.0 in
  let rec f time =
    if time /. msPerYear > 1.0
    then
      let suffix = if time /. msPerYear > 2.0 then "years" else "year" in
      ( (time /. msPerYear |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerYear)
    else if time /. msPerMonth > 1.0
    then
      let suffix = if time /. msPerMonth > 2.0 then "months" else "month" in
      ( (time /. msPerMonth |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerMonth)
    else if time /. msPerDay > 1.0
    then
      let suffix = if time /. msPerDay > 2.0 then "days" else "day" in
      ((time /. msPerDay |> int_of_float |> string_of_int) ^ " " ^ suffix ^ ", ")
      ^ f (mod_float time msPerDay)
    else if time /. msPerHour > 1.0
    then
      let suffix = if time /. msPerHour > 2.0 then "hours" else "hour" in
      ( (time /. msPerHour |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerHour)
    else if time /. msPerMinute > 1.0
    then
      let suffix = if time /. msPerMinute > 2.0 then "minutes" else "minute" in
      ((time /. msPerMinute |> int_of_float |> string_of_int) ^ " " ^ suffix)
      ^ f (mod_float time msPerMinute)
    else ""
  in
  let diff = f time in
  if diff = "" then "less than a minute" else diff


external formatDate : Js.Date.t * string -> string = "formatDate"
  [@@bs.val] [@@bs.scope "window"] [@@bs.scope "Dark"]

module Regex = struct
  type t = Js.Re.t

  type result = Js.Re.result

  let regex ?(flags = "g") s : Js.Re.t = Js.Re.fromStringWithFlags ~flags s

  let contains ~(re : Js.Re.t) (s : string) : bool = Js.Re.test_ re s

  let replace ~(re : Js.Re.t) ~(repl : string) (str : string) =
    Js.String.replaceByRe re repl str


  (* WARNING: Js.Re.result contains an array, consisting of the whole match
   * followed by any substring matches. It does _not_ return every possible
   * match; for that, you need to call Js.Re.exec_ until it returns None *)
  let matches ~(re : Js.Re.t) (s : string) : Js.Re.result option =
    Js.Re.exec_ re s


  let exactly ~(re : string) (s : string) : bool =
    contains ~re:(regex ("^" ^ re ^ "$")) s


  (* Returns a list of capture groups if the string is matched by the expression.
  The list head is the whole match, and tail contains all the matched capture groups.
  If nothing is matched then it returns an empty list
  *)
  let captures ~(re : Js.Re.t) (s : string) : string list =
    match Js.Re.exec_ re s with
    | Some m ->
        Js.Re.captures m
        |> Array.toList
        |> List.filterMap ~f:(fun group -> Js.Nullable.toOption group)
    | None ->
        []
end

module Namer = struct
  let animals =
    [ "addax"
    ; "agouti"
    ; "anteater"
    ; "armadillo"
    ; "babirusa"
    ; "banteng"
    ; "bear"
    ; "camel"
    ; "capybara"
    ; "cheetah"
    ; "chimpanzee"
    ; "chinchilla"
    ; "degu"
    ; "dog"
    ; "elephant"
    ; "fox"
    ; "gazelle"
    ; "gerenuk"
    ; "giraffe"
    ; "goat"
    ; "goral"
    ; "gorilla"
    ; "hedgehog"
    ; "hippopotamus"
    ; "hog"
    ; "hyena"
    ; "jaguar"
    ; "kangaroo"
    ; "kudu"
    ; "langur"
    ; "lemur"
    ; "leopard"
    ; "lion"
    ; "meerkat"
    ; "mongoose"
    ; "monkey"
    ; "mouse"
    ; "okapi"
    ; "opossum"
    ; "orangutan"
    ; "otter"
    ; "panda"
    ; "penguin"
    ; "pig"
    ; "porcupine"
    ; "prairiedog"
    ; "puma"
    ; "rabbit"
    ; "hamster"
    ; "rhinoceros"
    ; "saki"
    ; "seal"
    ; "sealion"
    ; "sheep"
    ; "sifaka"
    ; "sloth"
    ; "springhaas"
    ; "squirrel"
    ; "takin"
    ; "tamarin"
    ; "tiger"
    ; "wallaby"
    ; "woodchuck"
    ; "zebra"
    ; "zebu" ]


  let adjs =
    [ "abnormal"
    ; "angry"
    ; "arrogant"
    ; "bearded"
    ; "blathering"
    ; "bonkers"
    ; "cocky"
    ; "confused"
    ; "costumed"
    ; "cowardly"
    ; "crawly"
    ; "deep"
    ; "demanding"
    ; "disrespectful"
    ; "disturbing"
    ; "domesticated"
    ; "fighting"
    ; "flickering"
    ; "fluffy"
    ; "frozen"
    ; "greedy"
    ; "hairless"
    ; "harsh"
    ; "haunting"
    ; "hilarious"
    ; "hyperactive"
    ; "idiotic"
    ; "impertinent"
    ; "impressive"
    ; "infuriating"
    ; "insane"
    ; "insecure"
    ; "magical"
    ; "maniacal"
    ; "metal"
    ; "mischievous"
    ; "misunderstood"
    ; "nighttime"
    ; "offensive"
    ; "outnumbered"
    ; "rebellious"
    ; "shaky"
    ; "shivering"
    ; "sick"
    ; "sickened"
    ; "sinister"
    ; "slimy"
    ; "slippery"
    ; "startled"
    ; "startling"
    ; "sticky"
    ; "stubborn"
    ; "tactful"
    ; "talking"
    ; "territorial"
    ; "twisted"
    ; "underhanded"
    ; "vengeful"
    ; "violent"
    ; "wild"
    ; "yapping"
    ; "zippy" ]


  let generateFallbackName (space : string) : string =
    space ^ "_" ^ (() |> random |> string_of_int)


  let generateAnimalWithPersonality ?(space = "unknown") (_ : unit) : string =
    let m = Native.Random.range 0 (List.length animals) in
    let a = Native.Random.range 0 (List.length adjs) in
    match (List.getAt ~index:a adjs, List.getAt ~index:m animals) with
    | Some adj, Some animal ->
        adj ^ String.capitalize animal
    | _ ->
        generateFallbackName space
end

(** Returns the indefinite article (either "a" or "an") depending on whether `subject` starts with a vowel. Only works for English vowels right now. *)
let indefiniteArticleFor (subject : string) : string =
  match String.slice ~from:0 ~to_:1 subject with
  | "A" | "E" | "I" | "O" | "U" | "a" | "e" | "i" | "o" | "u" ->
      "an"
  | _ ->
      "a"


(* Obscures string keeping only last n characters, and replacing everything else with X.
 If string length < n, then returns X of string length  *)
let obscureString (s : string) : string =
  let len = String.length s in
  let n = Int.minimum 4 (len / 4) in
  let diff = len - n in
  let redactedLeft = String.repeat ~count:diff "X" in
  let visibleRight = String.dropLeft ~count:diff s in
  redactedLeft ^ visibleRight


let hideSecrets (secretValues : string list) (s : string) : string =
  List.fold secretValues ~initial:s ~f:(fun buildingStr secretVal ->
      (* We are doing this instead of Regex.replace because it fails secretValues with regex characters
    And Js.String.replace only replaces the first found string. *)
      buildingStr
      |> String.split ~on:secretVal
      |> String.join ~sep:(obscureString secretVal))
