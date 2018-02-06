module Clipboard exposing (..)


-- Dark
import Types exposing (..)
import Toplevel
import Pointer
import AST
import Entry

copy : Model -> Toplevel -> (Maybe Pointer) -> Modification
copy m tl mp =
  case Toplevel.asHandler tl of
    Nothing -> NoChange
    Just h ->
      case mp of
        Nothing -> CopyToClipboard (Just <| AST.toPD h.ast)
        Just p ->
          let pid = Pointer.idOf p
          in
              CopyToClipboard (AST.subData pid h.ast)

cut : Model -> Toplevel -> Pointer -> Modification
cut m tl p =
  let pid = Pointer.idOf p
  in
    case Toplevel.asHandler tl of
      Nothing -> NoChange
      Just h ->
        let newClipboard = AST.subData pid h.ast
            (_, newAst) = AST.deleteExpr p h.ast
        in
            Many [ CopyToClipboard (AST.subData pid h.ast)
                , RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
                        , FocusNext tl.id Nothing )
                ]

paste : Model -> Toplevel -> Pointer -> Modification
paste m tl p =
  case m.clipboard of
    Nothing -> NoChange
    Just pd ->
      let cloned = Toplevel.clonePointerData pd
      in
          case Toplevel.asHandler tl of
            Nothing -> NoChange
            Just h ->
              let newAst = AST.replace p cloned h.ast
              in
                  RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
                      , FocusNext tl.id Nothing)

peek : Model -> Clipboard
peek m =
  Maybe.map Toplevel.clonePointerData m.clipboard

newFromClipboard : Model -> Pos -> Modification
newFromClipboard m pos =
  let nid = Entry.tlid ()
      ast =
        case peek m of
          Nothing -> Hole (gid ())
          Just a ->
            case a of
              PExpr _ exp -> exp
              _ -> Hole (gid ())
      spec = Entry.newHandlerSpec ()
      handler = { ast = ast, spec = spec }
  in
      RPC ([SetHandler nid pos handler], FocusNext nid Nothing)


