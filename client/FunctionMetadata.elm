module FunctionMetadata exposing (..)

-- dark
import Types exposing (..)

allParamData : UserFunctionParameter -> List PointerData
allParamData ufp =
  [(PParamName ufp.name), (PParamTipe ufp.tipe)]

allData : UserFunctionMetadata -> List PointerData
allData ufm =
  (PFnName ufm.name) :: (List.concat (List.map allParamData ufm.parameters))

replaceMetadataField : PointerData -> PointerData -> UserFunctionMetadata -> UserFunctionMetadata
replaceMetadataField old new ufm = ufm

