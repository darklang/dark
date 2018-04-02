module Refactor exposing (extractFunction)

-- lib
import List.Extra as LE

-- Dark
import Types exposing (..)
import Util exposing (..)
import Toplevel as TL
import Pointer as P
import AST
import Blank
import Analysis
import Util

generateFnName : () -> String
generateFnName _ =
  "fn_" ++ (() |> Util.random |> toString)

convertTipe : Tipe -> Tipe
convertTipe tipe =
  case tipe of
    TIncomplete -> TAny
    TError -> TAny
    _ -> tipe

extractFunction : Model -> Toplevel -> PointerData -> Modification
extractFunction m tl p =
  if not (TL.isValidID tl (P.toID p))
  then
    NoChange
  else case p of
    PExpr body ->
      let pred = TL.getPrevBlank tl (Just p)
               |> Maybe.map P.toID
          name = generateFnName ()
          -- TODO: do actual analysis to figure out
          -- what vars are 'free'
          vars = AST.allData body
               |> List.filterMap
                 (\n ->
                   case n of
                     PExpr boe ->
                       case Blank.flattenFF boe of
                         Blank _ -> Nothing
                         Flagged _ _ _ _ _ -> Nothing
                         F id e ->
                           case e of
                             Variable name -> Just (id, name)
                             _ -> Nothing
                     _ -> Nothing)
                |> LE.uniqueBy
                  (\(_, name) -> name)

          paramExprs =
            List.map (\(_, name) -> F (gid ()) (Variable name)) vars
          replacement =
            PExpr (F (gid ()) (FnCall name paramExprs))
          h =
            deMaybe
            "PointerData is a PExpr and isValidID for this TL"
            (TL.asHandler tl)
          newAst =
            AST.replace p replacement h.ast
          newH = { h | ast = newAst }
          params =
            List.map
            (\(id, name) ->
              let tipe = Analysis.getTipeOf m tl.id id
                          |> Maybe.withDefault TAny
                          |> convertTipe
              in
                  { name = name
                  , tipe = tipe
                  , block_args = []
                  , optional = False
                  , description = ""
                  })
                  vars
          metadata =
            { name = name
            , parameters = params
            , description = ""
            , returnTipe = TAny
            , infix = False
            }
          newF =
            { tlid = gtlid ()
            , metadata = metadata
            , ast = AST.clone body
            }
      in
          RPC ([ SetFunction newF, SetHandler tl.id tl.pos newH ]
              , FocusExact tl.id (P.toID replacement))
    _ -> NoChange

