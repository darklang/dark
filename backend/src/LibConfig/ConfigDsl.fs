/// Parsers for environment variables
module LibConfig.ConfigDsl

let getEnv (name : string) : string option =
  let var = System.Environment.GetEnvironmentVariable name
  if var = null then None else Some var

let getEnvExn (name : string) : string =
  match getEnv name with
  | Some s -> s
  | None -> failwith $"Environment variable {name} not set"

// TODO review the usages of this --
// adjust to actually support different directories for CLI vs Cloud runtimes
let absoluteDir (name : string) : string =
  let dir = getEnvExn name

  if not (System.IO.Path.IsPathFullyQualified dir) then
    failwith ($"FAIL: {name} is not absolute")
  else
    $"{dir}/"

let absoluteDirOrCurrent (name : string) : string =
  match getEnv name with
  | None -> "./"
  | Some dir ->
    if not (System.IO.Path.IsPathFullyQualified dir) then
      failwith ($"FAIL: {name} is not absolute")
    else
      $"{dir}/"


let int (name : string) : int = getEnvExn name |> int

let uuid (name : string) : System.Guid = getEnvExn name |> System.Guid.Parse

let uuids (name : string) : List<System.Guid> =
  (getEnvExn name).Split(',')
  |> Array.toList
  |> List.map (fun s -> System.Guid.Parse(s))

let bool (name : string) : bool =
  match getEnvExn name with
  | "y" -> true
  | "n" -> false
  | v ->
    failwith $"Invalid env var value for {name}={v}. Allowed values are 'n' and 'y'."


let lowercase (name : string) (v : string) =
  if v = String.toLowercase v then
    v
  else
    failwith ($"Env vars must be lowercased but {name}={v} is not")

/// Basically the same as string, except it doesn't enforce being lowercase
let credentials (name : string) : string = getEnvExn name

let credentialsOption (name : string) : string option =
  let v = credentials name
  if String.toLowercase v = "none" then None else Some v

let string (name : string) : string = getEnvExn name |> lowercase name

let stringOption (name : string) : string option =
  let v = string name
  if v = "none" then None else Some v


let intOption (name : string) : int option =
  let v = stringOption name

  match v with
  | None -> None
  | Some s -> Some(int s)

/// Give a list of choices and values to return if the choice is found
let stringChoice name (options : (string * 'a) list) : 'a =
  let v = getEnvExn name |> lowercase name

  options
  |> List.find (fun (k, _) -> k = v)
  |> Option.map Tuple2.second
  |> Option.defaultWith (fun () ->
    let options = options |> List.map Tuple2.first |> String.concat ", "
    failwith $"Envvar is not a valid option: '{name}' not in [{options}]")
