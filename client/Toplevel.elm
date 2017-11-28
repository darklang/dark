module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import AST

getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe

replace : Model -> Toplevel -> Model
replace m tl =
  update m tl.id (\_ -> tl)

firstHole : Model -> TLID -> ID
firstHole m id =
  getTL m id |> .ast |> AST.listHoles |> List.head |> Maybe.withDefault (ID 3)

isBindHole : Model -> TLID -> ID -> Bool
isBindHole m tlid id =
  getTL m tlid |> .ast |> AST.listBindHoles |> List.member id

isHandlerSpecHole : Model -> TLID -> ID -> Bool
isHandlerSpecHole m tlid id =
  getTL m tlid |> handlerSpecHoles |> List.member id

handlerSpecHoles : Toplevel -> List ID
handlerSpecHoles tl =
  let e2l a =
        case a of
          Empty hid -> [hid]
          _ -> []
      spec = tl.handlerSpec
  in e2l spec.module_ ++ e2l spec.name ++ e2l spec.modifier

replaceHandlerSpecHole : ID -> String -> HandlerSpec -> HandlerSpec
replaceHandlerSpecHole id value hs =
  let rh a =
        case a of
          Empty hid ->
            if id == hid
            then Full value
            else a
          _ -> a
  in { name = rh hs.name
     , module_ = rh hs.module_
     , modifier = rh hs.modifier
     }


findNextHole : Toplevel -> ID -> ID
findNextHole tl cur =
  let astHoles = AST.listHoles tl.ast
      holes = astHoles ++ (handlerSpecHoles tl)
  in case (LE.dropWhile (\x -> x /= cur) holes) of
     cur :: next :: _ -> next
     [cur] -> holes |> List.head |> deMaybe
     [] -> ID 237


update : Model -> TLID -> (Toplevel -> Toplevel) -> Model
update m tlid f =
  let mapped = List.map (\t ->
        if t.id /= tlid
        then t
        else f t) m.toplevels
  in
      { m | toplevels = mapped }

move : TLID -> Int -> Int -> Model -> Model
move tlid xOffset yOffset m = update m tlid (moveTL xOffset yOffset)

moveTL : Int -> Int -> Toplevel -> Toplevel
moveTL xOffset yOffset tl =
  let newPos = { x = tl.pos.x + xOffset, y = tl.pos.y + yOffset }
  in
      { tl | pos = newPos }

