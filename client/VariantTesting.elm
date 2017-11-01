module VariantTesting exposing (parseVariantTestsFromQueryString, variantIsActive, toCSSClass)

import List.Extra as LE

-- dark
import Types exposing (..)

-- Quick and dirty client side only, code-managed variant testing thingy
-- would be much nicer if we could just read a config file from disk in the server
-- and push that info down to client, but that seems like a lot of work for now?

parseVariantTestsFromQueryString : String -> Maybe (List VariantTest)
parseVariantTestsFromQueryString s = case String.uncons s of
                                       Just ('?', rest) -> rest
                                                           |> String.split "&"
                                                           |> List.filterMap splitOnEquals
                                                           |> List.filterMap toVariantTest
                                                           |> uniqueTests
                                                           |> Just
                                       Nothing          -> Nothing
                                       _                -> Nothing

variantIsActive : Model -> VariantTest -> Bool
variantIsActive m vt = m.tests
                     |> List.member vt

toVariantTest : (String, Bool) -> Maybe VariantTest
toVariantTest s = case s of
                    (_, False) -> Nothing
                    (test, _)  -> case (String.toLower test) of
                                    _              -> Nothing

toCSSClass : VariantTest -> String
toCSSClass vt =
  let test =
        case vt of
          StubVariant -> "stub"
    -- _  -> "default" -- Please never do this, let the compiler tell you if you missed a variant
  in test ++ "-variant"

-- drops the second if we have a bunch of the same varian
uniqueTests : List VariantTest -> List VariantTest
uniqueTests xs = xs
               |> LE.uniqueBy (\x -> case x of
                                     StubVariant -> "SV") -- well this is lovely


splitOnEquals : String -> Maybe (String, Bool)
splitOnEquals s = if String.contains "=" s
                   then case (String.split "=" s) of
                          []  -> Nothing
                          [_] -> Nothing
                          x :: xs -> case (xs |> String.join "=" |> String.toLower) of
                                       "true"  -> Just (x, True)
                                       "1"     -> Just (x, True)
                                       "false" -> Just (x, False)
                                       _       -> Nothing
                   else Nothing

