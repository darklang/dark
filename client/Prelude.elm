module Prelude exposing (..)

-- dark
import Types exposing (..)
import Util

tlCursorID : TLID -> Int -> ID -- Generate ID for
tlCursorID tlid idx =
  let stringID = (toString (deTLID tlid)) ++ (toString idx)
      intID = Result.withDefault 0 (String.toInt stringID)
  in
    (ID intID)

unwrapCursorState : CursorState -> CursorState
unwrapCursorState s =
  case s of
    Dragging _ _ _ unwrap -> unwrap
    _ -> s

tlidOf : CursorState -> Maybe TLID
tlidOf s =
  case unwrapCursorState s of
    Selecting tlid _ -> Just tlid
    Entering entryCursor ->
      case entryCursor of
        Creating _ -> Nothing
        Filling tlid _ -> Just tlid
    Deselected -> Nothing
    Dragging _ _ _ _ -> Nothing

idOf : CursorState -> Maybe ID
idOf s =
  case unwrapCursorState s of
    Selecting _ id -> id
    Entering entryCursor ->
      case entryCursor of
        Creating _ -> Nothing
        Filling _ id  -> Just id
    Deselected -> Nothing
    Dragging _ _ _ _ -> Nothing



deID : ID -> Int
deID (ID i) = i

deTLID : TLID -> Int
deTLID (TLID i) = i

gid : () -> ID -- Generate ID
gid unit = ID (Util.random unit)

gtlid : () -> TLID -- Generate ID
gtlid unit = TLID (Util.random unit)

deMaybe : String -> Maybe a -> a
deMaybe msg x =
  case x of
    Just y -> y
    Nothing -> Debug.crash <| "deMaybe: got an error, expected a " ++ msg

assert : (a -> Bool) -> a -> a
assert fn a =
  if fn a
  then a
  else Debug.crash ("assertion failure: " ++ toString a)

impossible : String -> a -> a
impossible msg a =
  let appIsGentle = False in -- TODO: make the app a lil' gentler by reporting this event to a crash logger or something instead of savagely crashing poor elm's runtime ;(
  if appIsGentle
  then
    a
  else
    Debug.crash ("something impossible occurred: " ++ msg ++ " (would have returned: '" ++ toString a ++ "' under gentler circumstances)")
