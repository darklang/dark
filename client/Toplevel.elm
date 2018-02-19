module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import DB
import Types exposing (..)
import Util exposing (deMaybe)
import AST
import Pointer as P
import SpecTypes

getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe "getTL"

replace : Model -> Toplevel -> Model
replace m tl =
  update m tl.id (\_ -> tl)

isThread : Handler -> Pointer -> Bool
isThread h p =
  h.ast |> AST.listThreadHoles |> List.member (P.idOf p)

specHeaderPointers : Handler -> List Pointer
specHeaderPointers h =
  [ P.blankTo HTTPRoute h.spec.name
  , P.blankTo HTTPVerb h.spec.modifier
  ]

getSpec : Handler -> Pointer -> Maybe (BlankOr String)
getSpec h p =
  [h.spec.name, h.spec.modifier]
  |> List.filter (\spec -> blankOrID spec == P.idOf p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceHTTPVerbBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceHTTPVerbBlank id value hs =
  { hs | modifier = if blankOrID hs.modifier == id
                    then Filled (blankOrID hs.modifier) value
                    else hs.modifier
  }
replaceHTTPRouteBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceHTTPRouteBlank id value hs =
  { hs | name = if blankOrID hs.name == id
                then Filled (blankOrID hs.name) value
                else hs.name
  }


deleteHTTPRouteBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteHTTPRouteBlank p hs newID =
  { hs | name = if blankOrID hs.name == (P.idOf p)
                then Blank newID
                else hs.name
  }

deleteHTTPVerbBlank : Pointer -> HandlerSpec -> ID -> HandlerSpec
deleteHTTPVerbBlank p hs newID =
  { hs | modifier = if blankOrID hs.modifier == (P.idOf p)
                    then Blank newID
                    else hs.modifier
  }


allBlanks : Toplevel -> List Pointer
allBlanks tl =
  tl
  |> allPointers
  |> List.filter P.isBlank

blanksWhere : (Pointer -> Bool) -> Toplevel -> List Pointer
blanksWhere fn tl =
  tl
  |> allBlanks
  |> List.filter fn

specHeaderBlanks : Toplevel -> List Pointer
specHeaderBlanks = blanksWhere (P.ownerOf >> ((==) POSpecHeader))


allPointers : Toplevel -> List Pointer
allPointers tl =
  case tl.data of
    TLHandler h ->
      specHeaderPointers h
      ++ AST.listPointers h.ast
      ++ SpecTypes.listPointers h.spec.types
    TLDB db ->
      DB.listPointers db



specHeaders : Handler -> List Pointer
specHeaders h =
  [ P.blankTo HTTPRoute h.spec.name
  , P.blankTo HTTPVerb h.spec.modifier]

siblings : Toplevel -> Pointer -> List Pointer
siblings tl p =
  case tl.data of
    TLHandler h ->
      case getParentOf tl p of
       Just _ ->
         AST.siblings p h.ast
       Nothing ->
         specHeaders h
         ++ SpecTypes.listInputPointers h.spec.types
         ++ [AST.toP h.ast]
         ++ SpecTypes.listOutputPointers h.spec.types
    _ -> []

getNextSibling : Toplevel -> Pointer -> Pointer
getNextSibling tl p =
  case tl.data of
    TLHandler h ->
      let sibs = siblings tl p in
      sibs
      |> LE.elemIndex p
      |> Maybe.map ((+) 1)
      |> Maybe.map (\i -> i % List.length sibs)
      |> Maybe.andThen (\i -> LE.getAt i sibs)
      |> Maybe.withDefault p
    _ -> p

getPrevSibling : Toplevel -> Pointer -> Pointer
getPrevSibling tl p =
  case tl.data of
    TLHandler h ->
      let sibs = siblings tl p
          -- 'safe' to deMaybe as there's always at least
          -- one member in the array
          last = deMaybe "sibling" <| LE.last sibs
      in
      sibs
      |> LE.elemIndex p
      |> Maybe.map (\i -> i - 1)
      |> Maybe.andThen (\i -> LE.getAt i sibs)
      |> Maybe.withDefault last
    _ -> p

getNextBlank : Toplevel -> Predecessor -> Successor
getNextBlank tl pred =
  case pred of
    Just pred ->
      let ps = allPointers tl
          index = LE.elemIndex pred ps |> Maybe.withDefault (-1)
          remaining = List.drop (index+1) ps
          blanks = List.filter P.isBlank remaining in
      List.head blanks
    Nothing -> firstBlank tl

getFirstASTBlank : Toplevel -> Successor
getFirstASTBlank tl =
  tl
  |> blanksWhere (\p -> P.ownerOf p == POAst)
  |> List.head


getPrevBlank : Toplevel -> Pointer -> Predecessor
getPrevBlank tl next =
  let holes = allBlanks tl in
  holes
  |> LE.elemIndex next
  |> Maybe.map (\i -> i - 1)
  |> Maybe.andThen (\i -> LE.getAt i holes)

firstBlank : Toplevel -> Successor
firstBlank tl =
  tl |> allBlanks |> List.head

lastBlank : Toplevel -> Successor
lastBlank tl =
  tl |> allBlanks |> LE.last

update : Model -> TLID -> (Toplevel -> Toplevel) -> Model
update m tlid f =
  let mapped = List.map (\t ->
        if t.id /= tlid
        then t
        else f t) m.toplevels
  in
      { m | toplevels = mapped }

move : TLID -> Int -> Int -> Model -> Model
move tlid xOffset yOffset m = update m tlid (moveTL xOffset yOffset)

moveTL : Int -> Int -> Toplevel -> Toplevel
moveTL xOffset yOffset tl =
  let newPos = { x = tl.pos.x + xOffset, y = tl.pos.y + yOffset }
  in
      { tl | pos = newPos }


asHandler : Toplevel -> Maybe Handler
asHandler tl =
  case tl.data of
    TLHandler h -> Just h
    _ -> Nothing

asDB : Toplevel -> Maybe DB
asDB tl =
  case tl.data of
    TLDB h -> Just h
    _ -> Nothing

handlers : List Toplevel -> List Handler
handlers tls =
  List.filterMap asHandler tls

dbs : List Toplevel -> List DB
dbs tls =
  List.filterMap asDB tls

getParentOf : Toplevel -> Pointer -> Maybe Pointer
getParentOf tl p =
  case tl.data of
    TLHandler h ->
      AST.parentOf_ (P.idOf p) h.ast
      |> Maybe.map AST.toP
    _ -> Nothing

getChildrenOf : Toplevel -> Pointer -> List Pointer
getChildrenOf tl p =
  case tl.data of
    TLHandler h ->
      h.ast
      |> AST.childrenOf (P.idOf p)
    _ -> []

firstChild : Toplevel -> Pointer -> Maybe Pointer
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe Pointer
rootOf tl =
  case tl.data of
    TLHandler h ->
      Just <| AST.toP h.ast
    _ -> Nothing

isValidPointer : Toplevel -> Pointer -> Bool
isValidPointer tl p =
  case P.ownerOf p of
    POSpecHeader ->
      let handler = asHandler tl in
      case handler of
        Nothing -> False
        Just h ->
          List.member p (specHeaders h)
    POAst ->
      let handler = asHandler tl in
      case handler of
        Nothing -> False
        Just h ->
          List.member p (AST.listPointers h.ast)
    PODb ->
      let db = asDB tl in
      case db of
        Nothing -> False
        Just d ->
          List.member p (DB.listPointers d)
    POSpecType ->
      let handler = asHandler tl in
      case handler of
        Nothing -> False
        Just h ->
          List.member p (SpecTypes.listPointers h.spec.types)


clonePointerData : PointerData -> PointerData
clonePointerData pd =
  let replaceBlankOr nid bo =
      case bo of
        Blank _ -> Blank nid
        Filled _ a -> Filled nid a
  in
      case pd of
        PVarBind id vb ->
          let nid = gid ()
          in PVarBind nid (replaceBlankOr nid vb)
        PHTTPVerb id sp ->
          let nid = gid ()
          in PHTTPVerb nid (replaceBlankOr nid sp)
        PHTTPRoute id sp ->
          let nid = gid ()
          in PHTTPRoute nid (replaceBlankOr nid sp)
        PExpr id expr ->
          let (nid, ast) = AST.clone expr
          in PExpr nid ast
        PField id f ->
          let nid = gid ()
          in PField nid (replaceBlankOr nid f)
        PDBColName id cn -> pd
        PDBColType id ct -> pd
        PDarkType id dt -> Debug.crash "TODO clonePointerdata"
        PDarkTypeField id dt -> Debug.crash "TODO clonePointerdata"

