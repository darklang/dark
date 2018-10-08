let onWindow eventName decoder = subscription (MySub (eventName, decoder))

type 'msg mySub = MySub of string * 'msg decoder

let subMap func (MySub (eventName, decoder)) =
  MySub (eventName, Json.Decode.map func decoder)

type state = 'msg listener Belt.Map.String.t

type 'msg listener = {decoders: 'msg decoder list; pid: Process.id}

let groupByEventName =
  let go (MySub (eventName, decoder)) =
    Dict.update eventName (listify decoder)
  in
  let listify value =
    Maybe.map (List.cons value) >> Maybe.withDefault [value] >> Just
  in
  List.foldl go Dict.empty

let init = Task.succeed Dict.empty

type msg = {eventName: string; event: value}

let onEffects router newSubs oldState =
  let leftStep _ {pid} task = Process.kill pid |> Task.andThen (always task) in
  let bothStep eventName {pid} decoders =
    Task.map (Dict.insert eventName (Listener (decoders, pid)))
  in
  let rightStep eventName decoders =
    Task.andThen (fun state ->
        Process.spawn
          (Dom.LowLevel.onWindow eventName value
             (Platform.sendToSelf router << Msg eventName))
        |> Task.andThen (fun pid ->
               Task.succeed
                 (Dict.insert eventName (Listener (decoders, pid)) state) ) )
  in
  Dict.merge leftStep bothStep rightStep oldState (groupByEventName newSubs)
    (Task.succeed Dict.empty)

let onSelfMsg router {eventName; event} state =
  match Dict.get eventName state with
  | Just {decoders} ->
      let try_ decoder =
        match decodeValue decoder event with
        | Ok msg -> Just (Platform.sendToApp router msg)
        | Err err -> Nothing
      in
      List.filterMap try_ decoders
      |> Task.sequence
      |> Task.andThen (always (Task.succeed state))
  | Nothing -> Task.succeed state
