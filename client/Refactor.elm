module Refactor exposing (extractFunction, renameFunction)

-- lib
import List.Extra as LE
import Set

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
          lets = AST.allData body
               |> List.filterMap
                 (\n ->
                   case n of
                     PExpr boe ->
                       case Blank.flattenFF boe of
                         Blank _ -> Nothing
                         Flagged _ _ _ _ _ -> Nothing
                         F id e as expr ->
                           case e of
                             Let lhs rhs body-> Just expr
                             _ -> Nothing
                     _ -> Nothing)
          uses =
            lets
            |> List.map AST.usesOf
            |> List.concat
            |> List.map (Blank.toID >> deID)
            |> Set.fromList

          freeVars =
            AST.allData body
            |> List.filterMap
              (\n ->
                case n of
                  PExpr boe ->
                    case Blank.flattenFF boe of
                      Blank _ -> Nothing
                      Flagged _ _ _ _ _ -> Nothing
                      F id e ->
                        case e of
                          Variable name ->
                            if Set.member (deID id) uses
                            then Nothing
                            else Just (id, name)
                          _ -> Nothing
                  _ -> Nothing)
            |> LE.uniqueBy
              (\(_, name) -> name)

          paramExprs =
            List.map (\(_, name) -> F (gid ()) (Variable name)) freeVars
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
                  { name = F (gid ()) name
                  , tipe = F (gid ()) tipe
                  , block_args = []
                  , optional = False
                  , description = ""
                  })
                  freeVars
          metadata =
            { name = F (gid ()) name
            , parameters = params
            , description = ""
            , returnTipe = F (gid ()) TAny
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

renameFunction : Model -> UserFunction -> UserFunction -> List Op
renameFunction m old new =
  let renameFnCalls ast old new =
        let transformCall newName old =
              let transformExpr name old =
                    case old of
                      F id (FnCall _ params) ->
                        F id (FnCall name params)
                      _ ->
                        old
              in
                  case old of
                    PExpr e ->
                      PExpr (transformExpr newName e)
                    _ -> old
            (origName, calls) =
              case old.metadata.name of
                Blank _ -> (Nothing, [])
                Flagged _ _ _ _ _ -> (Nothing, [])
                F _ n ->
                  (Just n, AST.allCallsToFn n ast |> List.map PExpr)
            newName =
              case new.metadata.name of
                Blank _ -> Nothing
                Flagged _ _ _ _ _ -> Nothing
                F _ n -> Just n
        in
            case (origName, newName) of
              (Just o, Just r) ->
                List.foldr
                (\call acc -> AST.replace call (transformCall r call) acc)
                ast
                calls
              _ -> ast
  in
      let newHandlers =
            m.toplevels
            |> List.filterMap
              (\tl ->
                case TL.asHandler tl of
                  Nothing -> Nothing
                  Just h ->
                    let newAst = renameFnCalls h.ast old new
                    in
                        if newAst /= h.ast
                        then
                          Just (SetHandler tl.id tl.pos { h | ast = newAst })
                        else Nothing)
          newFunctions =
              m.userFunctions
              |> List.filterMap
                (\uf ->
                  let newAst = renameFnCalls uf.ast old new
                  in
                      if newAst /= uf.ast
                      then
                        Just (SetFunction { uf | ast = newAst })
                      else Nothing)
      in
          newHandlers ++ newFunctions


