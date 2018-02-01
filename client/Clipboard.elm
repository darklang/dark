module Clipboard exposing (..)


-- Dark
import Types exposing (..)
import Toplevel
import Pointer
import AST

copy : Model -> TLID -> Pointer -> Modification
copy m tlid p =
  let tl = Toplevel.getTL m tlid
      pid = Pointer.idOf p
  in
    case Toplevel.asHandler tl of
      Nothing -> NoChange
      Just h ->
        CopyToClipboard (AST.subData pid h.ast)

cut : Model -> TLID -> Pointer -> Modification
cut m tlid p =
  let tl = Toplevel.getTL m tlid
      pid = Pointer.idOf p
  in
    case Toplevel.asHandler tl of
      Nothing -> NoChange
      Just h ->
        let newClipboard = AST.subData pid h.ast
            (_, newAst) = AST.deleteExpr p h.ast
        in
            Many [ CopyToClipboard (AST.subData pid h.ast)
                , RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
                        , FocusNext tlid Nothing )
                ]

paste : Model -> TLID -> Pointer -> Modification
paste m tlid p =
  case m.clipboard of
    Nothing -> NoChange
    Just pd ->
      let tl = Toplevel.getTL m tlid
          cloned = Toplevel.clonePointerData pd
      in
          case Toplevel.asHandler tl of
            Nothing -> NoChange
            Just h ->
              let newAst = AST.replace p cloned h.ast
              in
                  RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
                      , FocusNext tlid Nothing)



