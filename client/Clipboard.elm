module Clipboard exposing (..)


-- Dark
import Types exposing (..)
import Toplevel as TL
import Pointer as P
import AST
import Entry

copy : Model -> Toplevel -> (Maybe Pointer) -> Modification
copy m tl mp =
  case TL.asHandler tl of
    Nothing -> NoChange
    Just h ->
      case mp of
        Nothing -> CopyToClipboard (Just <| AST.toPD (n2o h.ast))
        Just p ->
          let pid = P.idOf p
          in CopyToClipboard (AST.subData pid (n2o h.ast))

cut : Model -> Toplevel -> Pointer -> Modification
cut m tl p =
  let pid = P.idOf p
      pred = TL.getPrevBlank tl (Just p)
  in
    case TL.asHandler tl of
      Nothing -> NoChange
      Just h ->
        let newClipboard = AST.subData pid (n2o h.ast)
            newAst = AST.deleteExpr p (n2o h.ast) (gid ())
        in Many [ CopyToClipboard newClipboard
                , RPC ( [ SetHandler tl.id tl.pos { h | ast = o2n newAst } ]
                        , FocusNext tl.id pred)
                ]

paste : Model -> Toplevel -> Pointer -> Modification
paste m tl p =
  case m.clipboard of
    Nothing -> NoChange
    Just pd ->
      let cloned = TL.clonePointerData pd
      in case TL.asHandler tl of
           Nothing -> NoChange
           Just h ->
             let newAst = AST.replace p cloned (n2o h.ast)
             in RPC ( [ SetHandler tl.id tl.pos { h | ast = o2n newAst } ]
                    , FocusNext tl.id (Just (P.pdToP cloned)))

peek : Model -> Clipboard
peek m =
  Maybe.map TL.clonePointerData m.clipboard

newFromClipboard : Model -> Pos -> Modification
newFromClipboard m pos =
  let nid = gtlid ()
      ast =
        case peek m of
          Nothing -> Blank (gid ())
          Just a ->
            case a of
              PExpr _ exp -> exp
              _ -> Blank (gid ())
      spec = Entry.newHandlerSpec ()
      handler = { ast = ast, spec = spec }
  in
      RPC ([SetHandler nid pos handler], FocusNext nid Nothing)


