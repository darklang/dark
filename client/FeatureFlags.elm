module FeatureFlags exposing (..)

-- dark
import Types exposing (..)
import Toplevel as TL
import Util exposing (deMaybe)
import Pointer as P
import SpecHeaders
import SpecTypes
import Blank as B
import AST

start : Model -> Modification
start m =
  case unwrapState m.state of
    Selecting tlid (Just p) ->
      let tl = TL.getTL m tlid
          predecessor = TL.getPrevBlank tl (Just p)
          wrap op = RPC ([op], FocusNext tlid predecessor)
          maybeH = TL.asHandler tl
          db = TL.asDB tl
          id = P.toID p
          sff = Flagged "" 0 (B.new ()) (B.new ())
          dtff = Flagged "" 0 (B.new ()) (B.new ())
          eff = Flagged "" 0 (B.new ()) (B.new ())
          fff = Flagged "" 0 (B.new ()) (B.new ())
      in
      case P.typeOf p of
        VarBind -> NoChange
        EventName ->
          let h = deMaybe "maybeH - eventname" maybeH
              replacement = SpecHeaders.replaceEventName
                              id sff h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        EventModifier ->
          let h = deMaybe "maybeH - eventmodifier" maybeH
              replacement = SpecHeaders.replaceEventModifier
                              id sff h.spec
          in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        EventSpace ->
          let h = deMaybe "maybeH - eventspace" maybeH
              replacement = SpecHeaders.replaceEventSpace
                              id sff h.spec
          in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        Field ->
          let h = deMaybe "maybeH - field" maybeH
              ast = AST.replace p (PField fff) h.ast
          in
          wrap <| SetHandler tlid tl.pos { h | ast = ast }
        Expr ->
          let h = deMaybe "maybeH - expr" maybeH
              ast = AST.replace p (PExpr eff) h.ast
          in
          wrap <| SetHandler tlid tl.pos { h | ast = ast }
        DarkType ->
          let h = deMaybe "maybeH - httpverb" maybeH
              replacement = SpecTypes.replace p (PDarkType dtff) h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        DarkTypeField ->
          let h = deMaybe "maybeH - expr" maybeH
              replacement = SpecTypes.replace p (PDarkTypeField dtff) h.spec in
          wrap <| SetHandler tlid tl.pos { h | spec = replacement }
        DBColName -> NoChange
        DBColType -> NoChange


    _ -> NoChange

