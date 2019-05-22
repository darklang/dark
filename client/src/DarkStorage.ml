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
          {deployHash; url; lastUpdate = Js.Date.fromString lastUpdate; status}
          )
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
  let deploys =
    newDeploys @ oldDeploys
    (* sorts into reverse order *)
    |> List.sortBy ~f:(fun d -> Js.Date.getTime d.lastUpdate)
  in
  (* We get two messages from the backend for the same hash: Deploying and
   * Deployed. If we have both, we should only keep Deployed. This does that,
   * though not perfectly. If there are two consecutive deploys with the same
   * hash (eg a Deployed and a Deploying), we pick the deployed one. If there
   * are multiple deploys at the same time, these might overlap. Also this
   * reverses the list into the correct order. *)
  List.foldl ~init:[] deploys ~f:(fun d accum ->
      match accum with
      | prev :: rest ->
          if prev.deployHash = d.deployHash
          then if prev.status = Deployed then prev :: rest else d :: rest
          else d :: prev :: rest
      | _ ->
          d :: accum )
