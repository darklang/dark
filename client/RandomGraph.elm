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
import Autocomplete

makeRandomChange : Model -> Modification
makeRandomChange m =
  let id = G.gen_id ()
      r () = Util.random () % 100
      r0 = r ()
      nodes = m.nodes |> Dict.values |> List.filter (\n -> n.pos.x /= -42)
      numNodes = List.length nodes
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
    RPC ([AddValue id (Debug.log "starting: " (val ())) (Just m.center)], FocusNothing)

  else
    let randomIndex = Util.random () % numNodes
        n = nodes |> LE.getAt randomIndex |> deMaybe
    in
    if r0 > 93 -- delete something
    then
      RPC ([DeleteNode (Debug.log "delete " n.id)], FocusNothing)

    -- else if r0 > 80 -- change a param somewhere
    -- then
    else -- lets add a result
      let cursor = if r () > 50
                   then case G.findNextHole m n of
                          Nothing -> Filling n (ResultHole n)
                          Just hole -> Filling (Entry.nodeFromHole hole) hole
                   else Filling n (G.findHole n)
          -- pick a random (but appropriate) autocomplete value
          ac =
            m.complete
               |> Autocomplete.reset
               |> (\ac ->
                 case cursor of
                   Filling n (ResultHole _) ->
                     Autocomplete.forLiveValue n.liveValue ac
                   Filling _ (ParamHole _ p _) ->
                     Autocomplete.forParamType p.tipe ac
                   _ -> ac)
               |> Autocomplete.regenerate
          ac2 = List.foldl (\_ ac -> Autocomplete.selectDown ac) ac (List.range 1 (1 + (r () % 10)))
          selected =
            ac2
               |> Autocomplete.highlighted
               |> deMaybe
               |> Autocomplete.asName
          value = let choice = r () in
                  if choice > 90 then val ()
                  else
                  if choice > 70
                  then
                    if n.liveValue.tipe == "String" then "String::foreach"
                       else if n.liveValue.tipe == "List" then "List::foreach"
                       else "if"
                       else selected

      in Entry.submit m False (Debug.log "cursor" cursor) (Debug.log "adding" (value))

-- add a new something, connected to a node
-- pick a random node and pick "next" on it. Now add something that
-- fits. Going up? 50% new node, 50% existing node
-- Going down? 90% new node, 10% existing param if one exists?


