module Refactor exposing (extractFunction)

-- Dark
import Types exposing (..)
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
  case TL.asHandler tl of
    Nothing -> NoChange
    Just h ->
      case TL.find tl (P.toID p) of
        Nothing -> NoChange
        Just (PExpr body) ->
          let pred = TL.getPrevBlank tl (Just p)
                   |> Maybe.map P.toID
              name = generateFnName ()
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
              paramExprs =
                List.map (\_ -> Blank.new ()) vars
              replacement =
               PExpr (F (gid ()) (FnCall name paramExprs))
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
              RPC ( [ SetFunction newF, SetHandler tl.id tl.pos newH ]
              , FocusNext tl.id pred)
        _ -> NoChange
