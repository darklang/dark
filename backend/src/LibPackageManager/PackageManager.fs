/// Fetches package items from a web-based package manager, for CLI runtimes.
///
/// TODO: this currently assumes that the package items match the shape
/// of Dark types defined in @Darklang.LanguageTools.ProgramTypes
///
/// TODO: move this to something more CLI-focused,
/// and review if current usages are appropriate.
///
/// TODO: address: how can we best react to the shape of types at the corresponding endpoints changing?
///
/// At this point, we only expose the PT package manager over HTTP,
/// so the RT PM here just PT2RTs our way to success.
/// Longer-term, we'll have a Sqlite cache of packages here, and a generally
/// fancier setup, probably with lookups for both PT and RT stuff.
module LibPackageManager.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes

open LibPackageManager.Types

module EPT = ProgramTypes

module ET2PT = ExternalTypesToProgramTypes


let withCache (f : 'name -> Ply<Option<'value>>) =
  let cache = System.Collections.Concurrent.ConcurrentDictionary<'name, 'value>()
  fun (name : 'name) ->
    uply {
      let mutable cached = Unchecked.defaultof<'value>
      let inCache = cache.TryGetValue(name, &cached)
      if inCache then
        return Some cached
      else
        let! result = f name
        match result with
        | Some v -> cache.TryAdd(name, v) |> ignore<bool>
        | None -> ()
        return result
    }

let httpClient = new System.Net.Http.HttpClient() // CLEANUP pass this in as param? or mutate it externally?

let fetch
  (url : string)
  (decoder : SimpleJson.JsonDecoder<'serverType>)
  (f : 'serverType -> 'cachedType)
  : Ply<Option<'cachedType>> =
  uply {
    let! response = url |> httpClient.GetAsync

    let! responseStr = response.Content.ReadAsStringAsync()
    try
      if response.StatusCode = System.Net.HttpStatusCode.OK then
        debuG "responseStr" responseStr
        let deserializedMaybe =
          SimpleJson.deserialize<'serverType> decoder responseStr

        match deserializedMaybe with
        | Ok deserialized ->
          let cached = f deserialized
          return Some cached
        | Error e ->
          return
            Exception.raiseInternal
              "Failed to deserialize package item"
              [ "responseStr", responseStr; "url", url; "error", e ]
              null

      else if response.StatusCode = System.Net.HttpStatusCode.NotFound then
        return None
      else
        return
          Exception.raiseInternal
            "Failed to fetch package"
            [ "responseStr", responseStr; "url", url ]
            null
    with e ->
      return
        Exception.raiseInternal
          "Failed to deserialize package"
          [ "responseStr", responseStr; "url", url; "exception", e ]
          e
  }



let getById
  (baseUrl : string)
  (kind : string)
  (decoder : SimpleJson.JsonDecoder<'ServerType>)
  (f : 'ServerType -> 'ResponseType)
  (id : uuid)
  : Ply<Option<'ResponseType>> =
  let url = $"{baseUrl}/{kind}/get/{id}"
  fetch url decoder f


/// The baseUrl is expected to be something like
/// - https://dark-packages.darklang.io normally
/// - http://dark-packages.dlio.localhost:11001 for local dev
let rt (baseUrl : string) : RT.PackageManager =
  { getType =
      getById
        baseUrl
        "type"
        JsonDeserialization.ProgramTypes.PackageType.decoder
        (fun t -> t |> ET2PT.PackageType.toPT |> PT2RT.PackageType.toRT)
      |> withCache

    getFn =
      getById
        baseUrl
        "function"
        JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
        (fun f -> f |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT)
      |> withCache

    getConstant =
      getById
        baseUrl
        "constant"
        JsonDeserialization.ProgramTypes.PackageConstant.decoder
        (fun c -> c |> ET2PT.PackageConstant.toPT |> PT2RT.PackageConstant.toRT)
      |> withCache

    init = uply { return () } }


let findByName
  (baseUrl : string)
  (kind : string)
  (owner : string)
  (modules : List<string>)
  (name : string)
  : Ply<Option<uuid>> =
  let modules = modules |> String.concat "."
  let namestring = $"{owner}.{modules}.{name}"
  let url = $"{baseUrl}/{kind}/find/{namestring}"
  fetch url SimpleJson.Decoders.uuid identity


/// The baseUrl is expected to be something like
/// - https://dark-packages.darklang.io normally
/// - http://dark-packages.dlio.localhost:11001 for local dev
let pt (baseUrl : string) : PT.PackageManager =
  { findType =
      (fun (name : PT.PackageType.Name) ->
        findByName baseUrl "type" name.owner name.modules name.name)
      |> withCache

    findConstant =
      (fun (name : PT.PackageConstant.Name) ->
        findByName baseUrl "constant" name.owner name.modules name.name)
      |> withCache

    findFn =
      (fun (name : PT.PackageFn.Name) ->
        findByName baseUrl "function" name.owner name.modules name.name)
      |> withCache


    getType =
      getById
        baseUrl
        "type"
        JsonDeserialization.ProgramTypes.PackageType.decoder
        ET2PT.PackageType.toPT
      |> withCache

    getFn =
      getById
        baseUrl
        "function"
        JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
        ET2PT.PackageFn.toPT
      |> withCache

    getConstant =
      getById
        baseUrl
        "constant"
        JsonDeserialization.ProgramTypes.PackageConstant.decoder
        ET2PT.PackageConstant.toPT
      |> withCache

    init = uply { return () } }
