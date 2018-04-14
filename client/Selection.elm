module Selection exposing (..)

-- builtins
import Maybe

-- lib
import Maybe.Extra as ME
import List.Extra as LE

-- dark
import Types exposing (..)
import Toplevel as TL
import Analysis
import Pointer as P
import Blank as B
import Functions as Fns
import Navigation
import Util exposing (deMaybe)

-------------------------------
-- Cursors
-------------------------------
moveCursorBackInTime : Model -> TLID -> Modification
moveCursorBackInTime m selected =
  let maxCursor = List.length (Analysis.getAnalysisResults m selected) - 1
  in
    selected
    |> TL.getTL m
    |> (\tl -> SetCursor tl.id (min (tl.cursor + 1) maxCursor))

moveCursorForwardInTime : Model -> TLID -> Modification
moveCursorForwardInTime m selected =
  selected
  |> TL.getTL m
  |> (\tl -> SetCursor tl.id (max (tl.cursor - 1) 0))

-------------------------------
-- Toplevels
-------------------------------
selectNextToplevel : Model -> (Maybe TLID) -> Modification
selectNextToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\cur -> Util.listNextWrap cur tls)
  in
  case next of
    Just nextId -> Select nextId Nothing
    Nothing -> Deselect

selectPrevToplevel : Model -> (Maybe TLID) -> Modification
selectPrevToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\cur -> Util.listPreviousWrap cur tls)
  in
  case next of
    Just nextId -> Select nextId Nothing
    Nothing -> Deselect

-------------------------------
-- Move direction-wise
-------------------------------

-- This is not an easy problem.
-- We want to move to the _thing_ above us, to the right of us, etc.

-- Left and right are pretty straightforward, until you hit interesting
-- edge cases:
--   (1 |> + 2) + (4 |> + 5)
-- In this example, pressing right on 2 should go to 5. That pretty much
-- rules out a straightforward AST traversal, though we could try up and
-- down, it seems tricky to get right.
--
-- Up and down are pretty complicated even in simple cases:
--   L1: 4 + 5 + 6 + 7
--   L2: 485960020 + 8
-- From 8, if you press up you should go to 7. How can we know?
--
-- The most obvious implementation is to model a grid (just like a text
-- editor) and then use simple integer to go up/down using the column
-- numbers. However, this is subject to a lot of weird errors: what if
-- the gear or flag icons are showing for example? Or what if we change
-- the fonts, etc. We tried to simulate sizes before for a similar
-- problem, and kept getting it wrong.
--
-- So the easiest thing to get correct -- and, importantly, to keep
-- correct over time -- is to use the browser to figure this out. Take
-- the TL, draw it, get the sizes and positions of the elements and use
-- them to figure out what's "above" and "below".

type alias HtmlSizing = { x: Float
                        , y: Float
                        , width: Float
                        , height: Float
                        , top: Float
                        , right: Float
                        , bottom: Float
                        , left: Float
                        }
tlToSizes : Model -> TLID -> List (ID, HtmlSizing)
tlToSizes m tlid =
  Native.Size.positions (deTLID tlid)
  |> List.map
       (\obj -> (ID obj.id, { x = obj.x
                            , y = obj.y
                            , width  = obj.width
                            , height = obj.height
                            , top = obj.top
                            , bottom = obj.bottom
                            , left = obj.left
                            , right = obj.right
                            }))

moveUp : Model -> TLID -> (Maybe ID) -> Modification
moveUp m tlid mId =
  case mId of
    Nothing -> NoChange
    Just id ->
      let sizes = tlToSizes m tlid |> Debug.log "sizes"
          this = sizes
                 |> List.filter (\(oid, o) -> oid == id)
                 |> List.head
                 |> deMaybe "must be one of them"
                 |> Debug.log "this"
                 |> Tuple.second
          above = sizes
                  |> LE.minimumBy (\(id, o) ->
                    if o.top >= this.top -- y axis is inverted in browser
                    then 10000000000
                    else
                      let thisCenter = (this.left + this.right) / 2 |> Debug.log "thiscenter"

                          oCenter = (o.left + o.right) / 2 |> Debug.log "ocenter"
                          yDist = this.top - o.top |> Debug.log "ydist"
                          xDist = abs (oCenter - thisCenter) |> Debug.log "xdist"
                      in yDist * 100000 + xDist)
                  |> Debug.log "above"
                  |> Maybe.map Tuple.first
      in
      Select tlid above

moveDown : Model -> TLID -> (Maybe ID) -> Modification
moveDown m tlid mId =
  Select tlid mId

moveRight : Model -> TLID -> (Maybe ID) -> Modification
moveRight m tlid mId =
  Select tlid mId

moveLeft : Model -> TLID -> (Maybe ID) -> Modification
moveLeft m tlid mId =
  Select tlid mId


-------------------------------
-- Move AST-wide
-------------------------------
selectUpLevel : Model -> TLID -> (Maybe ID) -> Modification
selectUpLevel m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.andThen (TL.getParentOf tl)
  |> Maybe.map P.toID
  |> Select tlid

selectDownLevel : Model -> TLID -> (Maybe ID) -> Modification
selectDownLevel m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> ME.orElse (TL.rootOf tl)
  |> Maybe.andThen (TL.firstChild tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

selectNextSibling : Model -> TLID -> (Maybe ID) -> Modification
selectNextSibling m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.map (TL.getNextSibling tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

selectPreviousSibling : Model -> TLID -> (Maybe ID) -> Modification
selectPreviousSibling m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.map (TL.getPrevSibling tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

-------------------------------
-- Blanks
-------------------------------

selectNextBlank : Model -> TLID -> (Maybe ID) -> Modification
selectNextBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getNextBlank tl
  |> Maybe.map P.toID
  |> Select tlid

enterNextBlank : Model -> TLID -> (Maybe ID) -> Modification
enterNextBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getNextBlank tl
  |> Maybe.map (\pd -> Enter (Filling tlid (P.toID pd)))
  |> Maybe.withDefault NoChange

selectPrevBlank : Model -> TLID -> (Maybe ID) -> Modification
selectPrevBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getPrevBlank tl
  |> Maybe.map P.toID
  |> Select tlid

enterPrevBlank : Model -> TLID -> (Maybe ID) -> Modification
enterPrevBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getPrevBlank tl
  |> Maybe.map (\pd -> Enter (Filling tlid (P.toID pd)))
  |> Maybe.withDefault NoChange


-------------------------------
-- misc
-------------------------------


delete : Model -> TLID -> Maybe ID -> Modification
delete m tlid mId =
  case mId of
    Nothing ->
      let tl = TL.getTL m tlid
      in
      case tl.data of
        TLHandler _ -> Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
        TLDB _ -> Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
        TLFunc _ -> Error ("Cannot delete functions!")
    Just id ->
      let newID = gid ()
          wrapDB op = RPC ([op], FocusExact tlid newID)
          tl = TL.getTL m tlid
          pd = TL.findExn tl id
      in
      case P.typeOf pd of
        DBColType ->
          wrapDB <| SetDBColType tlid id ""
        DBColName ->
          wrapDB <| SetDBColName tlid id ""
        _ ->
          let newTL = TL.delete tl pd newID in
              case newTL.data of
                TLHandler h -> RPC ([SetHandler tlid tl.pos h], FocusExact tlid newID)
                TLFunc f -> RPC ([SetFunction f], FocusExact tlid newID)
                TLDB _ -> Debug.crash "pointer type mismatch - Selection.delete"


enter : Model -> TLID -> ID -> Modification
enter m tlid id =
  let tl = TL.getTL m tlid
      pd = TL.findExn tl id
      -- if it's a FF, enter a side of it, not the thing itself
      flat = P.exprmap (B.flattenFF) pd
  in
  if TL.getChildrenOf tl pd /= []
  then selectDownLevel m tlid (Just id)
  else
    case pd of
      pd -> Many [ Enter (Filling tlid (P.toID flat))
                 , AutocompleteMod (ACSetQuery (P.toContent pd))
                 ]

startEditingFn : Model -> Modification
startEditingFn m =
  case unwrapCursorState m.cursorState of
    Selecting tlid (Just id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
      in
          case pd of
            PExpr (F _ (FnCall name _)) ->
              editFunction (Fns.findByNameExn m name)
            _ ->
              Debug.crash "should only be called on an FnCall for a UserFunction"
    _ -> NoChange

editFunction : UserFunction -> Modification
editFunction uf =
  MakeCmd (Navigation.modifyUrl (Fns.urlForFn uf))

