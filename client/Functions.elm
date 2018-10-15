module Functions exposing (..)

-- lib
import List.Extra as LE

-- dark
import DontPort exposing ((@))
import Types exposing (..)
import Prelude exposing (..)
import Blank as B
import Pointer as P
import AST

ufpToP : UserFunctionParameter -> Maybe Parameter
ufpToP ufp =
    case (ufp.ufpName, ufp.ufpTipe) of
      (F _ name, F _ tipe) ->
        { paramName = name
        , paramTipe = tipe
        , paramBlock_args = ufp.ufpBlock_args
        , paramOptional = ufp.ufpOptional
        , paramDescription = ufp.ufpDescription
        } |> Just
      _ -> Nothing

ufmToF : UserFunctionMetadata -> Maybe Function
ufmToF ufm =
  let ps = List.filterMap ufpToP ufm.ufmParameters
      sameLength = (List.length ps) == (List.length ufm.ufmParameters)
  in
      case (ufm.ufmName, ufm.ufmReturnTipe, sameLength) of
        (F _ name, F _ tipe, True) ->
          { fnName = name
          , fnParameters = ps
          , fnDescription = ufm.ufmDescription
          , fnReturnTipe = tipe
          , fnInfix = ufm.ufmInfix
          , fnPreviewExecutionSafe = False
          , fnDeprecated = False
          } |> Just
        _ -> Nothing

find : Model -> TLID -> Maybe UserFunction
find m id =
  LE.find (\f -> id == f.ufTLID) m.userFunctions

upsert : Model -> UserFunction -> Model
upsert m f =
  case find m f.ufTLID of
    Just old ->
      { m | userFunctions =
        m.userFunctions
        |> List.filter (\uf -> uf.ufTLID /= old.ufTLID)
        |> (::) f
      }
    Nothing ->
      { m | userFunctions =
        f :: m.userFunctions
      }

findExn : Model -> TLID -> UserFunction
findExn m id =
  find m id |> deMaybe "Functions.findExn"


sameName : String -> UserFunction -> Bool
sameName name uf =
  case uf.ufMetadata.ufmName of
    F _ n -> n == name
    _ -> False


findByName : Model -> String -> Maybe UserFunction
findByName m s =
  LE.find (sameName s) m.userFunctions

findByNameExn : Model -> String -> UserFunction
findByNameExn m s =
  findByName m s |> deMaybe "Functions.findByNameExn"

paramData : UserFunctionParameter -> List PointerData
paramData ufp =
  [(PParamName ufp.ufpName), (PParamTipe ufp.ufpTipe)]

allParamData : UserFunction -> List PointerData
allParamData uf =
  List.concat (List.map paramData uf.ufMetadata.ufmParameters)

allData : UserFunction -> List PointerData
allData uf =
  [PFnName uf.ufMetadata.ufmName]
  @ (allParamData uf)
  @ AST.allData uf.ufAST

replaceFnName : PointerData -> PointerData -> UserFunction -> UserFunction
replaceFnName search replacement uf =
  let metadata = uf.ufMetadata
      sId = P.toID search
  in
      if B.toID metadata.ufmName == sId
      then
        let newMetadata =
              case replacement of
                PFnName new ->
                  { metadata | ufmName = (B.replace sId new metadata.ufmName) }
                _ -> metadata
        in
            { uf | ufMetadata = newMetadata }
      else
        uf

replaceParamName : PointerData -> PointerData -> UserFunction -> UserFunction
replaceParamName search replacement uf =
  let metadata = uf.ufMetadata
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
      if List.any (\p -> B.toID p == sId) paramNames
      then
        let newMetadata =
              case replacement of
                PParamName new ->
                  let newP =
                        metadata.ufmParameters
                        |> List.map (\p -> { p | ufpName = B.replace sId new p.ufpName })
                  in
                      { metadata | ufmParameters = newP }
                _ -> metadata
            newBody =
              let sContent =
                    case search of
                      PParamName d -> B.toMaybe d
                      _ -> impossible search
                  rContent =
                    case replacement of
                      PParamName d -> B.toMaybe d
                      _ -> impossible replacement
                  transformUse rep old =
                    case old of
                      PExpr (F _ _) ->
                        PExpr (F (gid ()) (Variable rep))
                      _ -> impossible old
              in
                  case (sContent, rContent) of
                    (Just o, Just r) ->
                      let uses = AST.uses o uf.ufAST |> List.map PExpr
                      in
                          List.foldr
                          (\use acc -> AST.replace use (transformUse r use) acc)
                          uf.ufAST
                          uses
                    _ -> uf.ufAST
        in
            { uf | ufMetadata = newMetadata, ufAST = newBody }
      else
        uf

replaceParamTipe : PointerData -> PointerData -> UserFunction -> UserFunction
replaceParamTipe search replacement uf =
  let metadata = uf.ufMetadata
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
      if List.any (\p -> B.toID p == sId) paramTipes
      then
        let newMetadata =
              case replacement of
                PParamTipe new ->
                  let newP =
                        metadata.ufmParameters
                        |> List.map (\p -> { p | ufpTipe = B.replace sId new p.ufpTipe })
                  in
                      { metadata | ufmParameters = newP }
                _ -> metadata
        in
            { uf | ufMetadata = newMetadata }
      else
          uf

replaceMetadataField : PointerData -> PointerData -> UserFunction -> UserFunction
replaceMetadataField old new uf =
  uf
  |> replaceFnName old new
  |> replaceParamName old new
  |> replaceParamTipe old new

extend : UserFunction -> UserFunction
extend uf =
  let newParam =
        { ufpName = B.new ()
        , ufpTipe = B.new ()
        , ufpBlock_args = []
        , ufpOptional = False
        , ufpDescription = ""
        }
      metadata = uf.ufMetadata
      newMetadata =
        { metadata | ufmParameters = (uf.ufMetadata.ufmParameters @ [newParam]) }
  in
      { uf | ufMetadata = newMetadata }


removeParameter : UserFunction -> UserFunctionParameter -> UserFunction
removeParameter uf ufp =
  let metadata = uf.ufMetadata
      params =
        List.filter (\p -> p /= ufp) metadata.ufmParameters
      newM =
        { metadata | ufmParameters = params }
  in
      { uf | ufMetadata = newM }
