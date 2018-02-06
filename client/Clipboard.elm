module Clipboard exposing (..)


-- Dark
import Types exposing (..)
import Toplevel
import Pointer
import AST

copy : Model -> Toplevel -> Pointer -> Modification
copy m tl p =
  let pid = Pointer.idOf p
  in
    case Toplevel.asHandler tl of
      Nothing -> NoChange
      Just h ->
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



