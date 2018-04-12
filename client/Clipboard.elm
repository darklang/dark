module Clipboard exposing (..)


-- Dark
import Types exposing (..)
import Toplevel as TL
import Pointer as P
import AST
import Entry
import Blank
import Util exposing (deMaybe)

copy : Model -> Toplevel -> (Maybe PointerData) -> Modification
copy m tl mp =
  case tl.data of
    TLDB _ ->
      NoChange
    TLHandler h ->
      case mp of
        Nothing -> CopyToClipboard (Just <| PExpr h.ast)
        Just p -> CopyToClipboard (TL.find tl (P.toID p))
    TLFunc f ->
      case mp of
        Nothing -> CopyToClipboard (Just <| PExpr f.ast)
        Just p -> CopyToClipboard (TL.find tl (P.toID p))

cut : Model -> Toplevel -> PointerData -> Modification
cut m tl p =
  let pid = P.toID p
      pred = TL.getPrevBlank tl (Just p)
             |> Maybe.map P.toID
  in
      case tl.data of
        TLDB _ ->
          NoChange
        TLHandler h ->
          let newClipboard = TL.find tl pid
              newH = TL.delete tl p (gid ())
                    |> TL.asHandler
                    |> deMaybe "cut"

          in Many [ CopyToClipboard newClipboard
                  , RPC ( [ SetHandler tl.id tl.pos newH ]
                          , FocusNext tl.id pred)
                  ]
        TLFunc f ->
          let newClipboard = TL.find tl pid
              newF = TL.delete tl p (gid ())
                    |> TL.asUserFunction
                    |> deMaybe "cut"

          in Many [ CopyToClipboard newClipboard
                  , RPC ( [ SetFunction newF ]
                          , FocusNext tl.id pred)
                  ]

paste : Model -> Toplevel -> ID -> Modification
paste m tl id =
  case m.clipboard of
    Nothing -> NoChange
    Just pd ->
      let cloned = TL.clonePointerData pd
      in
          case tl.data of
            TLDB _ ->
              NoChange
            TLHandler h ->
              let newAst = AST.replace (TL.findExn tl id) cloned h.ast
              in
                  RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
                  , FocusNext tl.id (Just (P.toID cloned)))
            TLFunc f ->
              let newAst = AST.replace (TL.findExn tl id) cloned f.ast
              in
                  RPC ( [ SetFunction { f | ast = newAst } ]
                  , FocusNext tl.id (Just (P.toID cloned)))

peek : Model -> Clipboard
peek m =
  Maybe.map TL.clonePointerData m.clipboard

newFromClipboard : Model -> Pos -> Modification
newFromClipboard m pos =
  let nid = gtlid ()
      ast =
        case peek m of
          Nothing -> Blank.new ()
          Just a ->
            case a of
              PExpr exp -> exp
              _ -> Blank.new ()
      spec = Entry.newHandlerSpec ()
      handler = { ast = ast, spec = spec }
  in
      RPC ([SetHandler nid pos handler], FocusNext nid Nothing)

