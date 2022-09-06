open Tc

let random = (a: unit): int => Native.Random.random(a)

let findIndex = (~f: 'a => bool, l: list<'a>): option<(int, 'a)> =>
  List.find(~f=((_, a)) => f(a), List.mapWithIndex(~f=Tuple2.make, l))

let listPrevious = (~value: 'a, l: list<'a>): option<'a> =>
  l
  |> List.elemIndex(~value)
  |> Option.map(~f=x => x - 1)
  |> Option.andThen(~f=(index: int) => List.getAt(~index, l))

let listNext = (~value: 'a, l: list<'a>): option<'a> =>
  l
  |> List.elemIndex(~value)
  |> Option.map(~f=x => x + 1)
  |> Option.andThen(~f=(index: int) => List.getAt(~index, l))

let listNextWrap = (~value: 'a, l: list<'a>): option<'a> =>
  l |> listNext(~value) |> Option.orElse(List.head(l))

let removeQuotes = (s: string): string =>
  if String.endsWith(~suffix="\"", s) && String.startsWith(~prefix="\"", s) {
    s |> String.dropLeft(~count=1) |> String.dropRight(~count=1)
  } else {
    s
  }

let humanReadableTimeElapsed = (time: float): string => {
  let msPerMinute = 60.0 *. 1000.0
  let msPerHour = msPerMinute *. 60.0
  let msPerDay = msPerHour *. 24.0
  let msPerMonth = msPerDay *. 30.0
  let msPerYear = msPerDay *. 365.0
  let rec f = time =>
    if time /. msPerYear > 1.0 {
      let suffix = if time /. msPerYear > 2.0 {
        "years"
      } else {
        "year"
      }
      (time /. msPerYear |> int_of_float |> string_of_int) ++
      (" " ++ (suffix ++ ", ")) ++
      f(mod_float(time, msPerYear))
    } else if time /. msPerMonth > 1.0 {
      let suffix = if time /. msPerMonth > 2.0 {
        "months"
      } else {
        "month"
      }
      (time /. msPerMonth |> int_of_float |> string_of_int) ++
      (" " ++ (suffix ++ ", ")) ++
      f(mod_float(time, msPerMonth))
    } else if time /. msPerDay > 1.0 {
      let suffix = if time /. msPerDay > 2.0 {
        "days"
      } else {
        "day"
      }
      (time /. msPerDay |> int_of_float |> string_of_int) ++
      (" " ++ (suffix ++ ", ")) ++
      f(mod_float(time, msPerDay))
    } else if time /. msPerHour > 1.0 {
      let suffix = if time /. msPerHour > 2.0 {
        "hours"
      } else {
        "hour"
      }
      (time /. msPerHour |> int_of_float |> string_of_int) ++
      (" " ++ (suffix ++ ", ")) ++
      f(mod_float(time, msPerHour))
    } else if time /. msPerMinute > 1.0 {
      let suffix = if time /. msPerMinute > 2.0 {
        "minutes"
      } else {
        "minute"
      }
      (time /. msPerMinute |> int_of_float |> string_of_int) ++
      (" " ++ suffix) ++
      f(mod_float(time, msPerMinute))
    } else {
      ""
    }

  let diff = f(time)
  if diff == "" {
    "less than a minute"
  } else {
    diff
  }
}

@val @scope("window") @scope("Dark")
external formatDate: ((Js.Date.t, string)) => string = "formatDate"

module Namer = {
  let animals = list{
    "addax",
    "agouti",
    "anteater",
    "armadillo",
    "babirusa",
    "banteng",
    "bear",
    "camel",
    "capybara",
    "cheetah",
    "chimpanzee",
    "chinchilla",
    "degu",
    "dog",
    "elephant",
    "fox",
    "gazelle",
    "gerenuk",
    "giraffe",
    "goat",
    "goral",
    "gorilla",
    "hedgehog",
    "hippopotamus",
    "hog",
    "hyena",
    "jaguar",
    "kangaroo",
    "kudu",
    "langur",
    "lemur",
    "leopard",
    "lion",
    "meerkat",
    "mongoose",
    "monkey",
    "mouse",
    "okapi",
    "opossum",
    "orangutan",
    "otter",
    "panda",
    "penguin",
    "pig",
    "porcupine",
    "prairiedog",
    "puma",
    "rabbit",
    "hamster",
    "rhinoceros",
    "saki",
    "seal",
    "sealion",
    "sheep",
    "sifaka",
    "sloth",
    "springhaas",
    "squirrel",
    "takin",
    "tamarin",
    "tiger",
    "wallaby",
    "woodchuck",
    "zebra",
    "zebu",
  }

  let adjs = list{
    "abnormal",
    "angry",
    "arrogant",
    "bearded",
    "blathering",
    "bonkers",
    "cocky",
    "confused",
    "costumed",
    "cowardly",
    "crawly",
    "deep",
    "demanding",
    "disrespectful",
    "disturbing",
    "domesticated",
    "fighting",
    "flickering",
    "fluffy",
    "frozen",
    "greedy",
    "hairless",
    "harsh",
    "haunting",
    "hilarious",
    "hyperactive",
    "idiotic",
    "impertinent",
    "impressive",
    "infuriating",
    "insane",
    "insecure",
    "magical",
    "maniacal",
    "metal",
    "mischievous",
    "misunderstood",
    "nighttime",
    "offensive",
    "outnumbered",
    "rebellious",
    "shaky",
    "shivering",
    "sick",
    "sickened",
    "sinister",
    "slimy",
    "slippery",
    "startled",
    "startling",
    "sticky",
    "stubborn",
    "tactful",
    "talking",
    "territorial",
    "twisted",
    "underhanded",
    "vengeful",
    "violent",
    "wild",
    "yapping",
    "zippy",
  }

  let generateFallbackName = (space: string): string =>
    space ++ ("_" ++ (() |> random |> string_of_int))

  let generateAnimalWithPersonality = (~space="unknown", _: unit): string => {
    let m = Native.Random.range(0, List.length(animals))
    let a = Native.Random.range(0, List.length(adjs))
    switch (List.getAt(~index=a, adjs), List.getAt(~index=m, animals)) {
    | (Some(adj), Some(animal)) => adj ++ String.capitalize(animal)
    | _ => generateFallbackName(space)
    }
  }
}

@ocaml.doc(
  " Returns the indefinite article (either \"a\" or \"an\") depending on whether `subject` starts with a vowel. Only works for English vowels right now. "
)
let indefiniteArticleFor = (subject: string): string =>
  switch String.slice(~from=0, ~to_=1, subject) {
  | "A" | "E" | "I" | "O" | "U" | "a" | "e" | "i" | "o" | "u" => "an"
  | _ => "a"
  }

/* Obscures string keeping only last n characters, and replacing everything else with X.
 If string length < n, then returns X of string length */
let obscureString = (s: string): string => {
  let len = String.length(s)
  let n = Int.minimum(4, len / 4)
  let diff = len - n
  let redactedLeft = String.repeat(~count=diff, "X")
  let visibleRight = String.dropLeft(~count=diff, s)
  redactedLeft ++ visibleRight
}

let hideSecrets = (s: string, secretValues: list<string>): string =>
  List.fold(secretValues, ~initial=s, ~f=(buildingStr, secretVal) =>
    /* We are doing this instead of Regex.replace because it fails secretValues with regex characters
     And Js.String.replace only replaces the first found string. */
    buildingStr |> String.split(~on=secretVal) |> String.join(~sep=obscureString(secretVal))
  )
