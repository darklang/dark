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


module Cache =
  module OnDisk =
    // TODO if the shape of these types change, or if we change the definitions of the things we're caching,
    // then this cache will be invalid. We should have a version number in the cache, and check it when we load.
    // This will be less of an issue when package things don't change.

    open System.IO
    open System.Runtime.Serialization.Formatters.Binary

    let baseTempPath = Path.Combine(Path.GetTempPath(), "darklang")
    let dirPath = Path.Combine(baseTempPath, "packages")

    let getFilePath (cacheId : string) (key : string) =
      let dirPath = Path.Combine(dirPath, cacheId)

      if not (Directory.Exists(dirPath)) then
        Directory.CreateDirectory(dirPath) |> ignore<DirectoryInfo>

      Path.Combine(dirPath, $"{key}.json")

    let saveToDisk (cacheId : string) (key : string) (value : 'value) =
      let filePath = getFilePath cacheId key
      let json = Json.Vanilla.serialize value
      File.WriteAllText(filePath, json)

    let loadFromDisk (cacheId : string) (key : string) : Option<'value> =
      let filePath = getFilePath cacheId key
      if File.Exists filePath  then
        try
          let json = File.ReadAllText filePath
          let value = Json.Vanilla.deserialize<'value> json
          Some value
        with _ ->
          None
      else
        None


  module InMemory =
    open System.Collections.Concurrent

    let createCache () = ConcurrentDictionary<'name, 'value>()

    let saveToMemory
      (cache : ConcurrentDictionary<'name, 'value>)
      (name : 'name)
      (value : 'value)
      =
      cache.TryAdd(name, value) |> ignore<bool>

    let loadFromMemory
      (cache : ConcurrentDictionary<'name, 'value>)
      (name : 'name)
      : Option<'value> =
      let mutable cached = Unchecked.defaultof<'value>
      let inCache = cache.TryGetValue(name, &cached)
      if inCache then Some cached else None


let withCache (cacheId : string) (f : 'name -> Ply<Option<'value>>) =
  let memoryCache = Cache.InMemory.createCache ()
  fun (name : 'name) ->
    uply {
      let key = name.ToString()
      // first, check in-mem cache
      match Cache.InMemory.loadFromMemory memoryCache name with
      | Some value -> return Some value
      | None ->
        // if that fails, check on-disk cache
        match Cache.OnDisk.loadFromDisk cacheId key with
        | Some value ->
          Cache.InMemory.saveToMemory memoryCache name value
          return Some value
        | None ->
          // otherwise, fetch and save to both
          let! result = f name
          match result with
          | Some v ->
            Cache.InMemory.saveToMemory memoryCache name v
            Cache.OnDisk.saveToDisk cacheId key v
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
      |> withCache "getTypeRT"

    getFn =
      getById
        baseUrl
        "function"
        JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
        (fun f -> f |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT)
      |> withCache "getFnRT"

    getConstant =
      getById
        baseUrl
        "constant"
        JsonDeserialization.ProgramTypes.PackageConstant.decoder
        (fun c -> c |> ET2PT.PackageConstant.toPT |> PT2RT.PackageConstant.toRT)
      |> withCache "getConstantRT"

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
      |> withCache "findTypePT"

    findConstant =
      (fun (name : PT.PackageConstant.Name) ->
        findByName baseUrl "constant" name.owner name.modules name.name)
      |> withCache "findConstantPT"

    findFn =
      (fun (name : PT.PackageFn.Name) ->
        findByName baseUrl "function" name.owner name.modules name.name)
      |> withCache "findFnPT"


    getType =
      getById
        baseUrl
        "type"
        JsonDeserialization.ProgramTypes.PackageType.decoder
        ET2PT.PackageType.toPT
      |> withCache "getTypePT"

    getFn =
      getById
        baseUrl
        "function"
        JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
        ET2PT.PackageFn.toPT
      |> withCache "getFnPT"

    getConstant =
      getById
        baseUrl
        "constant"
        JsonDeserialization.ProgramTypes.PackageConstant.decoder
        ET2PT.PackageConstant.toPT
      |> withCache "getConstantPT"

    init = uply { return () } }
