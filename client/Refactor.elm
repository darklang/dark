module Refactor exposing (..)

-- lib
import List.Extra as LE
import Set exposing (Set)

-- Dark
import Types exposing (..)
import Util
import Prelude exposing (..)
import Toplevel as TL
import Pointer as P
import AST
import Blank as B
import Analysis
import Util
import Nineteen.String as String

generateFnName : () -> String
generateFnName _ =
  "fn_" ++ (() |> Util.random |> String.fromInt)

convertTipe : Tipe -> Tipe
convertTipe tipe =
  case tipe of
    TIncomplete -> TAny
    TError -> TAny
    _ -> tipe

type WrapLoc = WLetRHS
             | WLetBody
             | WIfCond
             | WIfThen
             | WIfElse

wrap : WrapLoc -> Model -> Toplevel -> PointerData -> Modification
wrap wl m tl p =
  let wrapAst e ast wl =
        let (replacement, focus) =
          case wl of
            WLetRHS ->
              let lhs = B.new ()
                  replacement = PExpr (B.newF (Let lhs e (B.new ())))
              in
                  (replacement, FocusExact tl.id (B.toID lhs))
            WLetBody ->
              let lhs = B.new ()
                  replacement = PExpr (B.newF (Let lhs (B.new ()) e))
              in
                  (replacement, FocusExact tl.id (B.toID lhs))
            WIfCond ->
              let thenBlank = B.new ()
                  replacement =
                    PExpr (B.newF (If e thenBlank (B.new ())))
              in
                  (replacement, FocusExact tl.id (B.toID thenBlank))
            WIfThen ->
              let condBlank = B.new ()
                  replacement =
                    PExpr (B.newF (If condBlank e (B.new ())))
              in
                  (replacement, FocusExact tl.id (B.toID condBlank))
            WIfElse ->
              let condBlank = B.new ()
                  replacement =
                    PExpr (B.newF (If condBlank (B.new ()) e))
              in
                  (replacement, FocusExact tl.id (B.toID condBlank))
        in
            (AST.replace (PExpr e) replacement ast, focus)

  in
      case (p, tl.data) of
        (PExpr e, TLHandler h) ->
          let (newAst, focus) =
                wrapAst e h.ast wl
              newH =
                { h | ast = newAst }
          in
              RPC ([SetHandler tl.id tl.pos newH]
                  ,focus)
        (PExpr e, TLFunc f) ->
          let (newAst, focus) =
                wrapAst e f.ast wl
              newF =
                { f | ast = newAst }
          in
              RPC ([SetFunction newF]
                  ,focus)
        _ -> NoChange

toggleOnRail : Model -> Toplevel -> PointerData -> Modification
toggleOnRail m tl p =
  let new =
    case p of
      PExpr (F id (FnCall name exprs Rail)) ->
        PExpr (F id (FnCall name exprs NoRail))
      PExpr (F id (FnCall name exprs NoRail)) ->
        PExpr (F id (FnCall name exprs Rail))
      _ -> p
  in
      if p == new
      then NoChange
      else
        let newtl = TL.replace p new tl in
        RPC (TL.toOp newtl, FocusSame)



extractVariable : Model -> Toplevel -> PointerData -> Modification
extractVariable m tl p =
  let extractVarInAst e ast  =
      let varname = "var" ++ String.fromInt (Util.random())
          freeVariables =
            AST.freeVariables e
            |> List.map Tuple.second
            |> Set.fromList
          ancestors =
            AST.ancestors (B.toID e) ast
          lastPlaceWithSameVarsAndValues =
            ancestors
            |> LE.takeWhile
              (\elem ->
                let id = B.toID elem
                    availableVars =
                      Analysis.getCurrentAvailableVarnames m tl.id id
                      |> Set.fromList
                    allRequiredVariablesAvailable =
                      Set.diff freeVariables availableVars
                      |> Set.isEmpty
                    noVariablesAreRedefined =
                       freeVariables
                       |> Set.toList
                       |> List.all (not << (\v -> AST.isDefinitionOf v elem))
                in
                    allRequiredVariablesAvailable
                    && noVariablesAreRedefined)
            |> LE.last
          newVar = B.newF varname
      in
          case lastPlaceWithSameVarsAndValues of
            Just p ->
              let nbody =
                    AST.replace (PExpr e) (PExpr (B.newF (Variable varname))) p
                  nlet = B.newF (Let newVar e nbody)
              in
                  (AST.replace (PExpr p) (PExpr nlet) ast, B.toID newVar)
            Nothing ->
              -- something weird is happening because we couldn't find anywhere to
              -- extract to, we can just wrap the entire AST in a Let
              let newAST = AST.replace (PExpr e) (PExpr (B.newF (Variable varname))) ast
              in
                  (B.newF (Let newVar e newAST), B.toID newVar)
  in
      case (p, tl.data) of
        (PExpr e, TLHandler h) ->
          let (newAst, enterTarget) =
                extractVarInAst e h.ast
              newHandler =
                { h | ast = newAst }
          in
              Many [ RPC ([SetHandler tl.id tl.pos newHandler]
                         , FocusNoChange)
                   , Enter (Filling tl.id enterTarget)
                   ]
        (PExpr e, TLFunc f) ->
          let (newAst, enterTarget) =
                extractVarInAst e f.ast
              newF =
                { f | ast = newAst }
          in
              Many [ RPC ([SetFunction newF]
                         , FocusNoChange)
                   , Enter (Filling tl.id enterTarget)
                   ]
        _ -> NoChange

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
          freeVars =
            AST.freeVariables body
          paramExprs =
            List.map (\(_, name) -> F (gid ()) (Variable name)) freeVars
          replacement =
            PExpr (F (gid ()) (FnCall name paramExprs NoRail))
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
              let tipe = Analysis.getCurrentTipeOf m tl.id id
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
                      F id (FnCall _ params r) ->
                        F id (FnCall name params r)
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
                F _ n ->
                  (Just n, AST.allCallsToFn n ast |> List.map PExpr)
            newName =
              case new.metadata.name of
                Blank _ -> Nothing
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

isFunctionInExpr : String -> Expr -> Bool
isFunctionInExpr fnName expr =
  let maybeNExpr = B.asF expr
  in case maybeNExpr of
    Nothing -> False
    Just nExpr ->
      case nExpr of
        FnCall name list _ ->
          if name == fnName
            then True
            else List.any (isFunctionInExpr fnName) list
        If ifExpr thenExpr elseExpr ->
          List.any (isFunctionInExpr fnName) [ifExpr, thenExpr, elseExpr]
        Variable _ -> False
        Let _ a b ->
          List.any (isFunctionInExpr fnName) [a, b]
        Lambda _ ex ->
          isFunctionInExpr fnName ex
        Value _ -> False
        ObjectLiteral li ->
          let valuesMap = List.map Tuple.second li
          in List.any (isFunctionInExpr fnName) valuesMap
        ListLiteral li ->
          List.any (isFunctionInExpr fnName) li
        Thread li ->
          List.any (isFunctionInExpr fnName) li
        FieldAccess ex filed ->
          isFunctionInExpr fnName ex
        FeatureFlag _ cond a b ->
          (isFunctionInExpr fnName cond) ||
          (isFunctionInExpr fnName a) ||
          (isFunctionInExpr fnName b)

countFnUsage : Model -> String -> Int
countFnUsage m name =
  let usedIn = TL.all m
    |> List.filter (\tl ->
      case tl.data of
        TLHandler h -> isFunctionInExpr name h.ast
        TLDB _ -> False
        TLFunc f -> isFunctionInExpr name f.ast
    )
  in List.length usedIn

unusedDeprecatedFunctions : Model -> Set String
unusedDeprecatedFunctions m =
  m.builtInFunctions
  |> List.filter .deprecated
  |> List.map .name
  |> List.filter (\n -> (countFnUsage m n) == 0)
  |> Set.fromList

transformFnCalls : Model -> UserFunction -> (NExpr -> NExpr) -> List Op
transformFnCalls m uf f =
  let transformCallsInAst f ast old =
        let transformCall old =
              let transformExpr old =
                    case old of
                      F id (FnCall name params r) ->
                        F id (f (FnCall name params r))
                      _ ->
                        old
              in
                  case old of
                    PExpr e ->
                      PExpr (transformExpr e)
                    _ -> old
            (origName, calls) =
              case old.metadata.name of
                Blank _ -> (Nothing, [])
                F _ n ->
                  (Just n, AST.allCallsToFn n ast |> List.map PExpr)
        in
            case origName of
              Just _ ->
                List.foldr
                (\call acc -> AST.replace call (transformCall call) acc)
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
                    let newAst = transformCallsInAst f h.ast uf
                    in
                        if newAst /= h.ast
                        then
                          Just (SetHandler tl.id tl.pos { h | ast = newAst })
                        else Nothing)
          newFunctions =
              m.userFunctions
              |> List.filterMap
                (\uf ->
                  let newAst = transformCallsInAst f uf.ast uf
                  in
                      if newAst /= uf.ast
                      then
                        Just (SetFunction { uf | ast = newAst })
                      else Nothing)
      in
          newHandlers ++ newFunctions

addNewFunctionParameter : Model -> UserFunction -> List Op
addNewFunctionParameter m old =
  let fn e =
        case e of
          FnCall name params r ->
            FnCall name (params ++ [B.new ()]) r
          _ -> e
  in
      transformFnCalls m old fn

removeFunctionParameter : Model -> UserFunction -> UserFunctionParameter -> List Op
removeFunctionParameter m uf ufp =
  let indexInList =
        LE.findIndex (\p -> p == ufp) uf.metadata.parameters
        |> deMaybe "tried to remove parameter that does not exist in function"
      fn e =
        case e of
          FnCall name params r ->
            FnCall name (LE.removeAt indexInList params) r
          _ -> e
  in
      transformFnCalls m uf fn

generateEmptyFunction : () -> UserFunction
generateEmptyFunction _ =
  let funcName = generateFnName ()
      tlid = gtlid ()
      params = [
          { name = F (gid ()) "var"
          , tipe = F (gid ()) TAny
          , block_args = []
          , optional = True
          , description = ""
          }
        ]
      metadata = {
        name = F (gid ()) funcName
        , parameters = params
        , description = ""
        , returnTipe = F (gid ()) TAny
        , infix = False
      }
  in (UserFunction tlid metadata (Blank (gid ())))
