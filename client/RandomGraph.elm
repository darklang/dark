module RandomGraph exposing (..)

-- builtin
import Dict
import Maybe

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Graph as G
import Entry

makeRandomChange : Model -> Modification
makeRandomChange m =
  let id = G.gen_id ()
      r0 = Util.random () % 100
      r1 = Util.random () % 100
      r2 = Util.random () % 100
      numNodes = Dict.size m.nodes
      str = "astring"
      int _ = Util.random () % 30
      float _ = (Util.random () % 2341 |> toFloat) / 37.0
      bool _ = (Util.random () % 100) > 50
      primitive _ = case Util.random () % 4 of
        1 -> int () |> toString
        2 -> float () |> toString
        3 -> bool () |> toString |> String.toLower
        _ -> str |> toString
      val _ = case Util.random () % 5 of
                5 -> let len = Util.random () % 6
                     in List.range len 5 |> List.map (\_ -> primitive ())
                        |> String.join "," |> (\s -> "[" ++ s ++ "]")
                _ -> primitive ()


  in

  if numNodes == 0 -- start something
  then
    let _ = Debug.log "start something" in
    RPC ([AddValue id (val ()) (Just m.center)], FocusNothing)

  else
    let randomIndex = Util.random () % numNodes
        randomlySelectedNode =
          m.nodes |> Dict.values |> LE.getAt randomIndex |> deMaybe
    in
    if r0 > 93 -- delete something
    then
      let _ = Debug.log "delete something" in
      RPC ([DeleteNode randomlySelectedNode.id], FocusNothing)

    -- else if r0 > 80 -- change a param somewhere
    -- then
    else -- lets add a result
      let _ = Debug.log "add something" in
      let n = randomlySelectedNode
          cursor = if r1 > 50
                   then case G.findNextHole m n of
                          Nothing -> Filling n (ResultHole n)
                          Just hole -> Filling (Entry.nodeFromHole hole) hole
                   else Filling n (G.findHole n)
      in Entry.submit m False cursor (Debug.log "submitting" (val ()))

-- add a new something, connected to a node
-- pick a random node and pick "next" on it. Now add something that
-- fits. Going up? 50% new node, 50% existing node
-- Going down? 90% new node, 10% existing param if one exists?


