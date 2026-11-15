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

let int (name : string) : int = getEnvExn name |> int
