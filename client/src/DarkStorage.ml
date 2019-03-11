open Types
open Tc

module NewStaticDeployPush = struct
  let decode =
    let open Tea.Json.Decoder in
    let decodeStatus v =
      match Obj.magic v with
      | ["Deploying"] ->
          Tea_result.Ok Deploying
      | ["Deployed"] ->
          Tea_result.Ok Deployed
      | _ ->
          Tea_result.Error "Unable to decode deployStatus"
    in
    let decodeDeploy =
      map4
        (fun deployHash url lastUpdate status ->
          {deployHash; url; lastUpdate; status} )
        (field "deploy_hash" string)
        (field "url" string)
        (field "last_update" string)
        (field "status" (Decoder decodeStatus))
    in
    map (fun msg -> msg) (field "detail" decodeDeploy)


  let listen ~key tagger =
    Native.registerGlobal "newStaticDeploy" key tagger decode
end

let appendDeploy
    (newDeploys : staticDeploy list) (oldDeploys : staticDeploy list) :
    staticDeploy list =
  newDeploys @ oldDeploys
  |> List.sortBy ~f:(fun d -> d.lastUpdate)
  |> List.reverse
  |> List.uniqueBy ~f:(fun d -> d.deployHash)
