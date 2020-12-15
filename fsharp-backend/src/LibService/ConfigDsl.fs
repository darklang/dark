module LibService.ConfigDsl

(* Parsers for env vars *)

let getEnv (name : string) : string = System.Environment.GetEnvironmentVariable name

let absoluteDir (name : string) : string =
  let dir = getEnv name
  if not (System.IO.Path.IsPathFullyQualified dir) then
    failwith ($"FAIL: {name} is not absolute")
  else
    $"{dir}/"


let int (name : string) : int = getEnv name |> int

let bool (name : string) : bool =
  let v = getEnv name
  match v with
  | "y" -> true
  | "n" -> false
  | _ ->
      failwith
        $"Invalid env var value for {name}={v}. Allowed values are 'n' and 'y'."


let lowercase (name : string) (v : string) =
  if v = v.ToLower() then
    v
  else
    failwith ($"Env vars must be lowercased but {name}={v} is not")


let string (name : string) : string = getEnv name |> lowercase name

let stringOption (name : string) : string option =
  let v = string name
  if v = "none" then None else Some v


let int_option (name : string) : int option =
  let v = stringOption name
  match v with
  | None -> None
  | Some s -> Some(int s)


let stringChoice name (options : string list) : string =
  let v = getEnv name |> lowercase name
  if List.contains v options then
    v
  else
    let options = String.concat ", " options
    failwith $"Envvar is not a valid option: '{name}' not in [{options}]"

let password (name : string) : string = getEnv name
