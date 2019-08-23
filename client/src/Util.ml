open Tc

let random (a : unit) : int = Native.Random.random a

let findIndex ~(f : 'a -> bool) (l : 'a list) : (int * 'a) option =
  List.find ~f:(fun (_, a) -> f a) (List.indexedMap ~f:Tuple2.create l)


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
      ( (time /. msPerDay |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
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


module Regex = struct
  type t = Js.Re.t

  type result = Js.Re.result

  let regex s : Js.Re.t = Js.Re.fromStringWithFlags ~flags:"g" s

  let contains ~(re : Js.Re.t) (s : string) : bool = Js.Re.test_ re s

  let replace ~(re : Js.Re.t) ~(repl : string) (str : string) =
    Js.String.replaceByRe re repl str


  let matches ~(re : Js.Re.t) (s : string) : Js.Re.result option =
    Js.Re.exec_ re s


  let exactly ~(re : string) (s : string) : bool =
    contains ~re:(regex ("^" ^ re ^ "$")) s
end

module Namer = struct
  let animals =
    [ "addax"
    ; "agouti"
    ; "anteater"
    ; "armadilo"
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
    ; "kinkajou"
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
    ; "drunken"
    ; "fighting"
    ; "flickering"
    ; "flirting"
    ; "fluffy"
    ; "frozen"
    ; "godawful"
    ; "greedy"
    ; "hairless"
    ; "harsh"
    ; "haunting"
    ; "high-end"
    ; "hilarious"
    ; "hyperactive"
    ; "idiotic"
    ; "impertinent"
    ; "impressive"
    ; "indecent"
    ; "infuriating"
    ; "insane"
    ; "insecure"
    ; "magical"
    ; "maniacal"
    ; "massive"
    ; "medicated"
    ; "metal"
    ; "mischievous"
    ; "misunderstood"
    ; "nighttime"
    ; "offensive"
    ; "outnumbered"
    ; "rebellious"
    ; "sadistic"
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
    ; "tripping"
    ; "twisted"
    ; "underhanded"
    ; "unholy"
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
