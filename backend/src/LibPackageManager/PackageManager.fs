/// Fetches package items from a web-based package manager, for CLI runtimes.
///
/// TODO: this currently assumes that the package items match the shape
/// of Dark types defined in @Darklang.LanguageTools.ProgramTypes
///
/// TODO: move this to something more CLI-focused,
/// and review if current usages are appropriate.
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



// TODO: copy back to LibCloud/LibCloudExecution, or relocate somewhere central
// TODO: what should we do when the shape of types at the corresponding endpoints change?

/// The baseUrl is expected to be something like
/// - https://dark-packages.darklang.io normally
/// - http://dark-packages.dlio.localhost:11001 for local dev
let packageManager (baseUrl : string) : RT.PackageManager =
  let httpClient = new System.Net.Http.HttpClient() // CLEANUP pass this in as param? or mutate it externally?

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

  let fetchByName
    (kind : string)
    (owner : string)
    (modules : List<string>)
    (name : string)
    (decoder : SimpleJson.JsonDecoder<'serverType>)
    (f : 'serverType -> 'cachedType)
    : Ply<Option<'cachedType>> =
    let modules = modules |> String.concat "."
    let namestring = $"{owner}.{modules}.{name}"
    let url = $"{baseUrl}/{kind}/by-name/{namestring}"
    fetch url decoder f

  let fetchById
    (kind : string)
    (id : tlid)
    (decoder : SimpleJson.JsonDecoder<'serverType>)
    (f : 'serverType -> 'cachedType)
    : Ply<Option<'cachedType>> =
    let url = $"{baseUrl}/{kind}/by-id/{id}"
    fetch url decoder f


  { getType =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageType) : RT.PackageType.T =
          parsed |> ET2PT.PackageType.toPT |> PT2RT.PackageType.toRT
        fetchByName
          "type"
          name.owner
          name.modules
          name.name
          JsonDeserialization.ProgramTypes.PackageType.decoder
          conversionFn)

    getFn =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageFn.PackageFn) : RT.PackageFn.T =
          parsed |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT
        fetchByName
          "function"
          name.owner
          name.modules
          name.name
          JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
          conversionFn)

    getFnByTLID =
      withCache (fun tlid ->
        let conversionFn (parsed : EPT.PackageFn.PackageFn) : RT.PackageFn.T =
          parsed |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT
        fetchById
          "function"
          tlid
          JsonDeserialization.ProgramTypes.PackageFn.PackageFn.decoder
          conversionFn)

    getConstant =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageConstant) : RT.PackageConstant.T =
          parsed |> ET2PT.PackageConstant.toPT |> PT2RT.PackageConstant.toRT
        fetchByName
          "constant"
          name.owner
          name.modules
          name.name
          JsonDeserialization.ProgramTypes.PackageConstant.decoder
          conversionFn)

    init = uply { return () } }
