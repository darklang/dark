module Prelude exposing (..)

-- dark
import Types exposing (..)
import Util

--------------------------------------
-- CursorState
--------------------------------------

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


--------------------------------------
-- IDs
--------------------------------------
deID : ID -> Int
deID (ID i) = i

deTLID : TLID -> Int
deTLID (TLID i) = i

gid : () -> ID -- Generate ID
gid unit = ID (Util.random unit)

gtlid : () -> TLID -- Generate ID
gtlid unit = TLID (Util.random unit)


--------------------------------------
-- Crashing
--------------------------------------

deMaybe : String -> Maybe a -> a
deMaybe msg x =
  case x of
    Just y -> y
    Nothing -> impossible ("got Nothing but expected something", msg)

assert : (a -> Bool) -> a -> a
assert fn a =
  if fn a
  then a
  else impossible ("assertion failure", a)

-- `Impossible` crashes with the value provided.
-- Is it very obvious why?
--   impossible ()
-- Want a string message?
--   impossible "The widgets can't arrive in this order"
-- Show a value:
--   impossible myVar
-- Combine them:
--   impossible ("The widges can't arrive in this order:", myVar)
impossible : a -> b
impossible a =
  Debug.crash ("something impossible occurred: " ++ (toString a))


-- Like impossible but has a different semantic meaning. If you have a
-- value you _could_ continue with, consider this.
recoverable : a -> b -> b
recoverable msg val =
  let error = "An unexpected but recoverable error happened. "
            ++ "For now we crash. "
            ++ "Message: "
            ++ toString msg
            ++ "Value: "
            ++ toString val
      -- TODO: surface the error to the user and in rollbar and
      -- continue.
      _ = Debug.crash error
  in
  val

-- Like impossible but with the message TODO
todo : a -> b
todo a =
  Debug.crash ("TODO: " ++ (toString a))
