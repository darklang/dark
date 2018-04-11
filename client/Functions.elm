module Functions exposing (..)

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Blank as B
import Pointer as P
import AST
import Util exposing (deMaybe)

ufpToP : UserFunctionParameter -> Maybe Parameter
ufpToP ufp =
    case (ufp.name, ufp.tipe) of
      (F _ name, F _ tipe) ->
        { name = name
        , tipe = tipe
        , block_args = ufp.block_args
        , optional = ufp.optional
        , description = ufp.description
        } |> Just
      _ -> Nothing

ufmToF : UserFunctionMetadata -> Maybe Function
ufmToF ufm =
  let ps = List.filterMap ufpToP ufm.parameters
      sameLength = (List.length ps) == (List.length ufm.parameters)
  in
      case (ufm.name, ufm.returnTipe, sameLength) of
        (F _ name, F _ tipe, True) ->
          { name = name
          , parameters = ps
          , description = ufm.description
          , returnTipe = tipe
          , infix = ufm.infix
          } |> Just
        _ -> Nothing

find : Model -> TLID -> Maybe UserFunction
find m id =
  LE.find (\f -> id == f.tlid) m.userFunctions

findExn : Model -> TLID -> UserFunction
findExn m id =
  find m id |> deMaybe "Functions.findExn"


sameName : String -> UserFunction -> Bool
sameName name uf =
  case uf.metadata.name of
    F _ n -> n == name
    _ -> False


findByName : Model -> String -> Maybe UserFunction
findByName m s =
  LE.find (sameName s) m.userFunctions

findByNameExn : Model -> String -> UserFunction
findByNameExn m s =
  findByName m s |> deMaybe "Functions.findByNameExn"

urlForFn : UserFunction -> String
urlForFn uf =
  "/admin/ui#" ++ ("fn=" ++ (toString (deTLID uf.tlid)))

paramData : UserFunctionParameter -> List PointerData
paramData ufp =
  [(PParamName ufp.name), (PParamTipe ufp.tipe)]

allParamData : UserFunction -> List PointerData
allParamData uf =
  List.concat (List.map paramData uf.metadata.parameters)

allData : UserFunction -> List PointerData
allData uf =
  [PFnName uf.metadata.name]
  ++ (allParamData uf)
  ++ AST.allData uf.ast

replaceFnName : PointerData -> PointerData -> UserFunction -> UserFunction
replaceFnName search replacement uf =
  let metadata = uf.metadata
      sId = P.toID search
  in
      if B.within metadata.name sId
      then
        let newMetadata =
              case replacement of
                PFnName new ->
                  { metadata | name = (B.replace sId new metadata.name) }
                _ -> metadata
        in
            { uf | metadata = newMetadata }
      else
        uf

replaceParamName : PointerData -> PointerData -> UserFunction -> UserFunction
replaceParamName search replacement uf =
  let metadata = uf.metadata
      sId = P.toID search
      paramNames =
        uf
        |> allParamData
        |> List.filterMap
          (\p ->
            case p of
              PParamName n -> Just n
              _ -> Nothing)
  in
      if List.any (\p -> B.within p sId) paramNames
      then
        let newMetadata =
              case replacement of
                PParamName new ->
                  let newP =
                    metadata.parameters
                    |> List.map (\p -> { p | name = B.replace sId new p.name })
                  in
                      { metadata | parameters = newP }
                _ -> metadata
            newBody =
              let sContent =
                    case search of
                      PParamName d -> B.toMaybe d
                      _ -> Debug.crash "impossible"
                  rContent =
                    case replacement of
                      PParamName d -> B.toMaybe d
                      _ -> Debug.crash "impossible"
                  transformUse rep old =
                    case old of
                      PExpr e ->
                        case e of
                          F _ _ ->
                            PExpr (F (gid ()) (Variable rep))
                          _ ->
                            Debug.crash "impossible"
                      _ -> Debug.crash "impossible"
              in
                  case (sContent, rContent) of
                    (Just o, Just r) ->
                      let uses = AST.uses o uf.ast |> List.map PExpr
                      in
                          List.foldr
                          (\use acc -> AST.replace use (transformUse r use) acc)
                          uf.ast
                          uses
                    _ -> uf.ast
        in
            { uf | metadata = newMetadata, ast = newBody }
      else
        uf

replaceParamTipe : PointerData -> PointerData -> UserFunction -> UserFunction
replaceParamTipe search replacement uf =
  let metadata = uf.metadata
      sId = P.toID search
      paramTipes =
        uf
        |> allParamData
        |> List.filterMap
          (\p ->
            case p of
              PParamTipe t -> Just t
              _ -> Nothing)
  in
      if List.any (\p -> B.within p sId) paramTipes
      then
        let newMetadata =
              case replacement of
                PParamTipe new ->
                  let newP =
                    metadata.parameters
                    |> List.map (\p -> { p | tipe = B.replace sId new p.tipe })
                  in
                      { metadata | parameters = newP }
                _ -> metadata
        in
            { uf | metadata = newMetadata }
      else
          uf

replaceMetadataField : PointerData -> PointerData -> UserFunction -> UserFunction
replaceMetadataField old new uf =
  uf
  |> replaceFnName old new
  |> replaceParamName old new
  |> replaceParamTipe old new



