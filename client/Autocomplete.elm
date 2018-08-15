module Autocomplete exposing (..)

-- builtin
import Dict
import Json.Decode as JSD
import Dom.Scroll
import Task

-- lib
import List.Extra as LE
-- import Maybe.Extra as ME
-- import String.Extra as SE

-- dark
import Prelude exposing (..)
import Util exposing (int2letter, letter2int)
import Types exposing (..)
import Functions
import Runtime as RT
import Pointer as P
import Analysis
import Toplevel as TL
import AST
import Commands

----------------------------
-- Focus
----------------------------

-- show the prev 5
-- obvi this should use getClientBoundingBox, but that's tough in Elm
height : Int -> Int
height i = if i < 4
           then 0
           else 14 * (i - 4)

focusItem : Int -> Cmd Msg
focusItem i = Dom.Scroll.toY "autocomplete-holder" (i |> height |> toFloat)
            |> Task.attempt FocusAutocompleteItem


----------------------------
-- External: utils
----------------------------

findFunction : Autocomplete -> String -> Maybe Function
findFunction a name =
  LE.find (\f -> f.name == name) a.functions

isStringEntry : Autocomplete -> Bool
isStringEntry a = String.startsWith "\"" a.value

isSmallStringEntry : Autocomplete -> Bool
isSmallStringEntry a =
  isStringEntry a && not (isLargeStringEntry a)

isLargeStringEntry : Autocomplete -> Bool
isLargeStringEntry a =
  isStringEntry a && String.contains "\n" a.value

getValue : Autocomplete -> String
getValue a =
  case highlighted a of
    Just item -> asName item
    Nothing -> a.value

sharedPrefix2 : String -> String -> String
sharedPrefix2 l r =
  case (String.uncons l, String.uncons r) of
    (Just (l1, lrest), Just (r1, rrest)) ->
      if l1 == r1 then
        String.fromChar l1 ++ sharedPrefix2 lrest rrest
      else
        ""
    _ -> ""

sharedPrefixList : List String -> String
sharedPrefixList strs =
  case List.head strs of
    Nothing -> ""
    Just s -> List.foldl sharedPrefix2 s strs

-- Find the shared prefix of all the possible suggestions (eg "List::")
sharedPrefix : Autocomplete -> String
sharedPrefix a =
  a.completions
  |> List.concat
  |> List.map asName
  |> sharedPrefixList

containsOrdered : String -> String -> Bool
containsOrdered needle haystack =
  case String.uncons needle of
    Just (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
        && containsOrdered newneedle (haystack
                                        |> String.split char
                                        |> List.drop 1
                                        |> String.join char)
    Nothing -> True

-- returns (indent, suggestion, search), where:
--  - indent is the string that occurs before the match
--  - suggestion is the match rewritten with the search
--  - search is the search rewritten to match the suggestion
--
-- Returns no suggestion or indent for an OmniAction
--
compareSuggestionWithActual : Autocomplete -> String -> (String, String, String)
compareSuggestionWithActual a actual =
  case highlighted a of
    Just (ACOmniAction _) -> ("", "", actual)
    _ ->
      let suggestion = sharedPrefix a
      in
        case String.indexes (String.toLower actual) (String.toLower suggestion) of
          [] -> ("", suggestion, actual)
          index :: _ ->
            let prefix = String.slice 0 index suggestion
                suffix = String.slice (index + String.length actual) (String.length suggestion) suggestion
            in
              (prefix, prefix ++ actual ++ suffix, actual)


----------------------------
-- Autocomplete state
----------------------------

empty : Autocomplete
empty = init [] False

nonAdminFunctions : List Function -> List Function
nonAdminFunctions fns =
  fns

init : List Function -> Bool -> Autocomplete
init fns isAdmin =
  let functions =
        if isAdmin
        then fns
        else nonAdminFunctions fns
  in
  { functions = functions
  , admin = isAdmin
  , completions = [[],[],[],[]]
  , allCompletions = []
  , index = -1
  , value = ""
  , tipe = Nothing
  , target = Nothing
  , isCommandMode = False
  }

reset : Model -> Autocomplete -> Autocomplete
reset m a =
  let userFunctionMetadata =
        m.userFunctions
        |> List.map .metadata
        |> List.filterMap Functions.ufmToF
      functions =
        m.builtInFunctions
        |> List.filter
          (\f -> not (List.member f.name (List.map .name userFunctionMetadata)))
        |> List.append userFunctionMetadata
  in
      init functions a.admin |> regenerate m

numCompletions : Autocomplete -> Int
numCompletions a =
  a.completions |> List.concat |> List.length

selectDown : Autocomplete -> Autocomplete
selectDown a = let max_ = numCompletions a
                   max = Basics.max max_ 1
                   new = (a.index + 1) % max
               in
                 { a | index = new }

selectUp : Autocomplete -> Autocomplete
selectUp a = let max = numCompletions a - 1 in
             { a | index = if a.index <= 0
                           then max
                           else a.index - 1
             }

-- Implementation:
-- n The autocomplete list should include:
--    y all imported functions
--    y restricted by types that are allowed
--    y allowed field names
--    n library names
--    y case-insensitive
-- n order by most likely, offer other alternatives below
--   n slight typos
--   n slight typeos
-- y Press enter to select
-- y Press right to fill as much as is definitive

setQuery : String -> Autocomplete -> Autocomplete
setQuery q a = refilter q a

appendQuery : String -> Autocomplete -> Autocomplete
appendQuery str a =
  let q = if isStringEntry a
          then String.dropRight 1 a.value ++ str ++ "\""
          else a.value ++ str
  in setQuery q a

highlighted : Autocomplete -> Maybe AutocompleteItem
highlighted a = LE.getAt a.index (List.concat a.completions)

documentationForItem : AutocompleteItem -> Maybe String
documentationForItem aci =
  case aci of
    ACFunction f ->
      if String.length f.description /= 0
      then Just f.description
      else Nothing
    ACCommand c -> Just (c.doc ++ " (" ++ c.shortcut ++ ")")
    _ -> Nothing

setTarget : Model -> Maybe (TLID, PointerData) -> Autocomplete -> Autocomplete
setTarget m t a =
  { a | target = t }
  |> regenerate m

update : Model -> AutocompleteMod -> Autocomplete -> Autocomplete
update m mod a =
  (case mod of
     ACSetQuery str -> setQuery str a
     ACAppendQuery str -> appendQuery str a
     ACReset -> reset m a
     ACSelectDown -> selectDown a
     ACSelectUp -> selectUp a
     ACSetTarget target -> setTarget m target a
     ACRegenerate -> regenerate m a
     ACEnableCommandMode -> enableCommandMode m a
  )

------------------------------------
-- Commands
------------------------------------
enableCommandMode : Model -> Autocomplete -> Autocomplete
enableCommandMode m a =
  { a | isCommandMode = True }

------------------------------------
-- Dynamic Items
------------------------------------

isDynamicItem : AutocompleteItem -> Bool
isDynamicItem item =
  case item of
    ACLiteral _ -> True
    ACOmniAction _ -> True
    _ -> False

isStaticItem : AutocompleteItem -> Bool
isStaticItem item = not (isDynamicItem item)

qLiteral : String -> Maybe AutocompleteItem
qLiteral s =
  if RT.isLiteral s
  then Just (ACLiteral s)
  else
    -- isLiteral only works for the full word
    if String.length s > 0
    then
      if String.startsWith (String.toLower s) "null"
      then Just (ACLiteral "null")
      else if String.startsWith (String.toLower s) "true"
      then Just (ACLiteral "true")
      else if String.startsWith (String.toLower s) "false"
      then Just (ACLiteral "false")
      else Nothing
    else Nothing


qNewDB : String -> Maybe AutocompleteItem
qNewDB s =
  if String.length s >= 3
      && Util.reExactly "[A-Z][a-zA-Z0-9_-]+" s
      -- annoying to offer a DB when looking for HTTP handler
      && s /= "HTTP"
      && s /= "HTT"
  then Just (ACOmniAction (NewDB s))
  else Nothing

qHTTPHandler : String -> Maybe AutocompleteItem
qHTTPHandler s =
  if (String.length s) == 0
  then Just (ACOmniAction NewHTTPHandler)
  else Nothing

qHTTPRoute : String -> Maybe AutocompleteItem
qHTTPRoute s =
  if String.startsWith "/" s
  then Just (ACOmniAction (NewHTTPRoute s))
  else Nothing

qEventSpace : String -> Maybe AutocompleteItem
qEventSpace s =
  if Util.reExactly "[A-Z]+" s
  then Just (ACOmniAction (NewEventSpace s))
  else Nothing

toDynamicItems : Bool -> String -> List AutocompleteItem
toDynamicItems isOmni query =
  let always = [qLiteral]
      omni =
        if isOmni
        then [ qNewDB, qHTTPHandler, qHTTPRoute, qEventSpace ]
        else []
      items = always ++ omni
  in
  items
  |> List.map (\aci -> aci query)
  |> List.filterMap identity

withDynamicItems : Maybe Target -> String -> List AutocompleteItem -> List AutocompleteItem
withDynamicItems target query acis =
  let new = toDynamicItems (target == Nothing) query
      withoutDynamic = List.filter isStaticItem acis
  in
  new ++ withoutDynamic

------------------------------------
-- Create the list
------------------------------------

regenerate : Model -> Autocomplete -> Autocomplete
regenerate m a =
  { a | allCompletions = generateFromModel m a }
  |> refilter a.value


refilter : String -> Autocomplete -> Autocomplete
refilter query old  =
  -- add or replace the literal the user is typing to the completions
  let fudgedCompletions = withDynamicItems old.target query old.allCompletions

      newCompletions = filter fudgedCompletions query
      newCount = newCompletions |> List.concat |> List.length

      oldHighlight = highlighted old
      oldHighlightNewPos =
        oldHighlight
        |> Maybe.andThen (\oh ->
             LE.elemIndex oh (List.concat newCompletions))

      index =
        -- Clear the highlight conditions
        if query == "" &&
        -- when we had previously highlighted something due to any actual match
        ((old.index /= -1 && old.value /= query)
        -- or this condition previously held and nothing has changed
        || (old.index == -1))
        then
          -1
        else
          -- If an entry is highlighted, and you press another
          -- valid key for that entry, keep it highlighted
          case oldHighlightNewPos of
            Just i -> i
            -- If an entry vanishes, highlight 0
            Nothing ->
              -- if nothing matches, highlight nothing
              if newCount == 0
              then -1
              -- we matched something but its gone, go to top of
              -- list
              else 0
  in { old | index = index
           , completions = newCompletions
           , value = query }

filter : List AutocompleteItem -> String -> List (List AutocompleteItem)
filter list query =
  let lcq = query |> String.toLower

      stringify i = (if 1 >= String.length lcq
                     then asName i
                     else asString i)
                    |> Util.replace "⟶" "->"


      -- split into different lists
      (dynamic, candidates0) = List.partition isDynamicItem list

      (candidates1, notSubstring) =
        List.partition (stringify
                        >> String.toLower
                        >> String.contains lcq
                       ) candidates0

      (startsWith, candidates2) =
        List.partition (stringify
                        >> String.startsWith query
                       ) candidates1

      (startsWithCI, candidates3) =
        List.partition (stringify
                        >> String.toLower
                        >> String.startsWith lcq
                        ) candidates2

      (substring, substringCI ) =
        List.partition (stringify
                        >> String.contains query
                        ) candidates3

      stringMatch =
        List.filter (asName
                     >> String.toLower
                     >> containsOrdered lcq)
          notSubstring

  in
  [ dynamic
  , startsWith, startsWithCI
  , substring, substringCI
  , stringMatch ]



generateFromModel : Model -> Autocomplete -> List AutocompleteItem
generateFromModel m a =
  let lv =
        case a.target of
          Nothing -> Nothing
          Just (tlid, p) ->
            TL.get m tlid
            |> Maybe.andThen TL.asHandler
            |> Maybe.map .ast
            |> Maybe.andThen (AST.getValueParent p)
            |> Maybe.map P.toID
            |> Maybe.andThen (Analysis.getLiveValue m tlid)
            -- don't filter on incomplete values
            |> Maybe.andThen (\lv -> if lv.tipe == TIncomplete
                                     then Nothing
                                     else Just lv)
      fields =
        case lv of
          Just lv ->
            case (a.target, lv.tipe) of
              (Just (_, p), TObj) ->
                if P.typeOf p == Field
                then jsonFields lv.json
                else []
              _ -> []
          Nothing -> []

      isExpression =
        case a.target of
          Just (_, p) -> P.typeOf p == Expr
          Nothing -> True

      isThreadMember =
        case a.target of
          Nothing -> False
          Just (tlid, p) ->
            TL.get m tlid
            |> Maybe.andThen TL.asHandler
            |> Maybe.map .ast
            |> Maybe.andThen (AST.parentOf_ (P.toID p))
            |> Maybe.map
              (\e ->
                case e of
                  F _ (Thread _) -> True
                  _ -> False)
            |> Maybe.withDefault False

      paramTipeForTarget =
        case a.target of
          Nothing -> Nothing
          Just (tlid, p) ->
            TL.get m tlid
            |> Maybe.andThen TL.asHandler
            |> Maybe.map .ast
            |> Maybe.andThen
              (\ast ->
                AST.getParamIndex ast (P.toID p))
            |> Maybe.andThen
              (\(name, index) ->
                a.functions
                |> LE.find (\f -> name == f.name)
                |> Maybe.map .parameters
                |> Maybe.andThen
                  (LE.getAt index)
                |> Maybe.map .tipe)

      -- functions
      funcList = if isExpression then a.functions else []
      functions =
        funcList
        |> List.filter
           (\{returnTipe} ->
              case a.tipe of
                Just t -> RT.isCompatible returnTipe t
                Nothing ->
                  case paramTipeForTarget of
                    Just t ->
                      RT.isCompatible returnTipe t
                    Nothing -> True)
        |> List.filter
          (\fn ->
             case lv of
               Just {tipe} ->
                 if isThreadMember
                 then
                   Nothing /= findCompatibleThreadParam fn tipe
                 else
                   Nothing /= findParamByType fn tipe
               Nothing -> True)
        |> List.map ACFunction

      extras =
        case a.target of
          Just (tlid, p) ->
            case P.typeOf p of
              -- autocomplete HTTP verbs if the handler is in the HTTP
              -- event space
              EventModifier ->
                case TL.spaceOf (TL.getTL m tlid) of
                  Just HSHTTP ->
                    [ "GET"
                    , "POST"
                    , "PUT"
                    , "DELETE"
                    , "PATCH"
                    ]
                  Just HSCron ->
                    [ "Daily"
                    , "Weekly"
                    , "Fortnightly"
                    , "Every 1hr"
                    , "Every 12hrs"
                    ]
                  Just HSOther -> []
                  Just HSEmpty -> []
                  Nothing -> []
              EventSpace ->
                ["HTTP"
                ,"CRON"
                ]
              DBColType ->
                let builtins =
                  [ "String"
                  , "Int"
                  , "Boolean"
                  , "Float"
                  , "Title"
                  , "Url"
                  , "Date"
                  , "Password"
                  , "UUID"
                  ]
                    dbs =
                      m.toplevels
                      |> TL.dbs
                      |> List.map .name
                    simple =
                      builtins ++ dbs
                    compound =
                      List.map
                        (\s -> "[" ++ s ++ "]")
                        simple
                in
                    simple ++ compound

              DarkType ->
                [ "Any"
                , "Empty"
                , "String"
                , "Int"
                , "{"
                ]
              ParamTipe ->
                [ "Any"
                , "String"
                , "Int"
                , "Boolean"
                , "Float"
                , "Date"
                , "Obj"
                , "Block"
                , "Char"
                , "List"
                ]
              _ -> []
          _ -> []

      varnames = Analysis.varnamesFor m a.target
      keywords =
        if isExpression
        then
          List.map ACKeyword [KLet, KIf, KLambda]
        else
          []
      regular =
        List.map ACExtra extras
        ++ List.map ACVariable varnames
        ++ keywords
        ++ functions
        ++ fields

      commands =
        List.map ACCommand Commands.commands
    in
        if a.isCommandMode
        then commands
        else regular


asName : AutocompleteItem -> String
asName aci =
  case aci of
    ACFunction {name} -> name
    ACField name -> name
    ACVariable name -> name
    ACExtra name -> name
    ACCommand command -> (":" ++ command.name)
    ACLiteral lit -> lit
    ACOmniAction ac ->
      case ac of
        NewDB name -> "Create new database: " ++ name
        NewHTTPHandler -> "Create new HTTP handler"
        NewHTTPRoute name -> "Create new HTTP handler for " ++ name
        NewEventSpace name -> "Create new " ++ name ++ " handler"
    ACKeyword k ->
      case k of
        KLet -> "let"
        KIf -> "if"
        KLambda -> "lambda"


asTypeString : AutocompleteItem -> String
asTypeString item =
  case item of
    ACFunction f -> f.parameters
                    |> List.map .tipe
                    |> List.map RT.tipe2str
                    |> String.join ", "
                    |> (\s -> "(" ++ s ++ ") ->  " ++ (RT.tipe2str f.returnTipe))
    ACField _ -> "field"
    ACVariable _ -> "variable"
    ACExtra _ -> ""
    ACCommand _ -> ""
    ACLiteral lit ->
      let tipe = lit |> RT.tipeOf |> RT.tipe2str in
      tipe ++ " literal"
    ACOmniAction _ -> ""
    ACKeyword _ -> "keyword"

asString : AutocompleteItem -> String
asString aci =
  asName aci ++ asTypeString aci

-- parse the json, take the list of keys, add a . to the front of it
-- TODO: this needs refactoring
jsonFields : String -> List AutocompleteItem
jsonFields json =
  json
    |> JSD.decodeString (JSD.dict JSD.value)
    |> Result.toMaybe
    |> deMaybe "json decode result"
    |> Dict.keys
    |> List.map ACField

findCompatibleThreadParam : Function -> Tipe -> Maybe Parameter
findCompatibleThreadParam {parameters} tipe =
  parameters
  |> List.head
  |> Maybe.andThen
      (\fst ->
        if RT.isCompatible fst.tipe tipe
        then Just fst
        else Nothing)

findParamByType : Function -> Tipe -> Maybe Parameter
findParamByType {parameters} tipe =
  parameters
  |> LE.find (\p -> RT.isCompatible p.tipe tipe)

---------------------------
-- Modifications
---------------------------
selectSharedPrefix: Autocomplete -> Modification
selectSharedPrefix ac =
  let sp = sharedPrefix ac in
  if sp == "" then NoChange
  else
    AutocompleteMod <| ACSetQuery sp

