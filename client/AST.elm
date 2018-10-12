module AST exposing (..)

-- builtin
import List

-- lib
import List.Extra as LE

-- dark
import DontPort exposing ((@), (^))
import Types exposing (..)
import Prelude exposing (..)
import Util
import Pointer as P
import Blank as B
import Set


-------------------------
-- Generic
-------------------------

traverse : (Expr -> Expr) -> Expr -> Expr
traverse fn expr =
  case expr of
    Blank _ -> expr
    F id nexpr ->
      F id
        (case nexpr of
          Value _ -> nexpr
          Variable _ -> nexpr

          Let lhs rhs body ->
            Let lhs (fn rhs) (fn body)

          If cond ifbody elsebody ->
            If (fn cond) (fn ifbody) (fn elsebody)

          FnCall name exprs r ->
            FnCall name (List.map fn exprs) r

          Lambda vars lexpr ->
            Lambda vars (fn lexpr)

          Thread exprs ->
            Thread (List.map fn exprs)

          FieldAccess obj field ->
            FieldAccess (fn obj) field

          ObjectLiteral pairs ->
            pairs
            |> List.map (\(k,v) -> (k, fn v))
            |> ObjectLiteral

          ListLiteral elems ->
            ListLiteral (List.map fn elems)

          FeatureFlag msg cond a b ->
            FeatureFlag msg (fn cond) (fn a) (fn b))

-------------------------
-- Children
-------------------------
children : Expr -> List PointerData
children expr =
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value _ -> []
        Variable _ -> []
        If cond ifbody elsebody ->
          [PExpr cond, PExpr ifbody, PExpr elsebody]
        FnCall name exprs _ ->
          List.map PExpr exprs
        Lambda vars lexpr ->
          (List.map PVarBind vars) @ [PExpr lexpr]
        Thread exprs ->
          List.map PExpr exprs
        FieldAccess obj field ->
          [PExpr obj, PField field]
        Let lhs rhs body ->
          [PVarBind lhs, PExpr rhs, PExpr body]
        ObjectLiteral pairs ->
          pairs
          |> List.map (\(k, v) -> [PKey k, PExpr v])
          |> List.concat
        ListLiteral elems ->
          List.map PExpr elems
        FeatureFlag msg cond a b ->
          [PFFMsg msg, PExpr cond, PExpr a, PExpr b]

-- Look through an AST for the expr with the id, then return it's
-- children.
childrenOf : ID -> Expr -> List PointerData
childrenOf pid expr =
  let co = childrenOf pid in
  if pid == B.toID expr
  then
    children expr
  else
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value _ -> []
        Variable _ -> []
        Let lhs rhs body ->
          co body @ co rhs

        If cond ifbody elsebody ->
          co cond @ co ifbody @ co elsebody

        FnCall name exprs _ ->
          List.map co exprs |> List.concat

        Lambda vars lexpr ->
          co lexpr

        Thread exprs ->
          List.map co exprs |> List.concat

        FieldAccess obj field ->
          co obj

        ObjectLiteral pairs ->
          pairs
          |> List.map Tuple.second
          |> List.map co
          |> List.concat

        ListLiteral pairs ->
          pairs
          |> List.map co
          |> List.concat

        FeatureFlag msg cond a b ->
          co cond @ co a @ co b


-------------------------
-- Parents
-------------------------
parentOf_ : ID -> Expr -> Maybe Expr
parentOf_ eid expr =
  let po = parentOf_ eid
      -- the `or` of all items in the list
      poList xs = xs
                  |> List.map po
                  |> List.filterMap identity
                  |> List.head
  in
  if List.member eid (children expr |> List.map P.toID)
  then Just expr
  else
    case expr of
      Blank _ -> Nothing
      -- not really sure what to do here
      F id nexpr ->
        case nexpr of
          Value _ -> Nothing
          Variable _ -> Nothing
          Let lhs rhs body ->
            poList [rhs, body]

          If cond ifbody elsebody ->
            poList [cond, ifbody, elsebody]

          FnCall name exprs _ ->
            poList exprs

          Lambda vars lexpr ->
            po lexpr

          Thread exprs ->
            poList exprs

          FieldAccess obj field ->
            po obj

          ListLiteral exprs ->
            poList exprs

          ObjectLiteral pairs ->
            -- we don't check the children because it's done up top
            pairs
            |> List.map Tuple.second
            |> poList

          FeatureFlag msg cond a b ->
            poList [cond, a, b]

parentOf : ID -> Expr -> Expr
parentOf id ast =
  deMaybe "parentOf" <| parentOf_ id ast



-------------------------
-- Thread stuff
-------------------------
listThreadBlanks : Expr -> List ID
listThreadBlanks expr =
  let r = listThreadBlanks
      rList : List Expr -> List ID
      rList exprs =
        exprs
        |> List.map listThreadBlanks
        |> List.concat
      rn nexpr =
        case nexpr of
          Value v -> []
          Variable name -> []

          Let lhs rhs body -> r rhs @ r body
          FnCall name exprs _ -> rList exprs
          Lambda vars body -> r body
          FieldAccess obj _ -> r obj

          If cond ifbody elsebody ->
            r cond @ r ifbody @ r elsebody

          Thread exprs ->
            let (blanks, filled) = List.partition B.isBlank exprs
                blankids = List.map B.toID blanks
                subExprsBlankids = rList filled
            in blankids @ subExprsBlankids

          ObjectLiteral pairs ->
            pairs
            |> List.map Tuple.second
            |> rList

          ListLiteral exprs ->
            rList exprs

          FeatureFlag _ cond a b ->
            r cond @ r a @ r b

  in case expr of
      Blank _ -> []
      F _ f -> rn f

closeThreads : Expr -> Expr
closeThreads expr =
  -- Close all threads
  case expr of
    F id (Thread exprs) ->
      let addBlank =
            case exprs of
              [] -> False
              [e] -> False
              (F _ (FnCall _ _ _) :: _) -> False
              _ -> True
          newExprs = List.filter B.isF exprs
                     |> List.map closeThreads
          adjusted =
            case newExprs of
              -- if an fncall moved into the first slot, we need to add a
              -- blank in front.
              F id_ (FnCall name args r) :: rest ->
                if addBlank
                then
                  [F id_ (FnCall name (B.new () :: args) r)] @ rest
                else
                  [F id_ (FnCall name args r)] @ rest
              _ -> newExprs
      in
      case adjusted of
        [] -> Blank id
        [e] -> e
        _ -> F id (Thread adjusted)
    _ -> traverse closeThreads expr

closeObjectLiterals : Expr -> Expr
closeObjectLiterals expr =
  case expr of
    F id (ObjectLiteral pairs) ->
      pairs
      |> List.filterMap (\(k,v) ->
                          if B.isBlank k && B.isBlank v
                          then Nothing
                          else Just (k, (closeObjectLiterals v)))
      |> (\l -> if l /= [] then l else [(B.new (), B.new ())])
      |> ObjectLiteral
      |> (\x -> F id x)
    _ -> traverse closeObjectLiterals expr

closeListLiterals : Expr -> Expr
closeListLiterals expr =
  case expr of
    F id (ListLiteral exprs) ->
      let exprs2 = List.map closeListLiterals exprs
          exprs3 = List.filter B.isF exprs2
      in
      F id (ListLiteral (exprs3 @ [B.new ()]))
    _ -> traverse closeObjectLiterals expr

closeBlanks : Expr -> Expr
closeBlanks expr =
  expr
  |> closeThreads
  |> closeObjectLiterals
  |> closeListLiterals

-- Find the child with the id `at` in the thread, and add a blank after it.
extendThreadChild : ID -> Expr -> List Expr -> List Expr
extendThreadChild at blank threadExprs =
  List.foldr (\e list ->
                if (B.toID e) == at
                then e :: blank :: list
                else e :: list)
             []
             threadExprs

-- extends thread at pos denoted by ID, if ID is in a thread
maybeExtendThreadAt : ID -> Expr -> Expr -> Expr
maybeExtendThreadAt id blank expr =
  case expr of
    F tid (Thread exprs) ->
      let newExprs = extendThreadChild id blank exprs
                     |> List.map (maybeExtendThreadAt id blank)
      in F tid (Thread newExprs)
    _ -> traverse (maybeExtendThreadAt id blank) expr


-- take an expression, and if
-- * it is a thread, add a blank at the end
-- * it is part of a thread, insert a blank just after the expr
-- * if it is not part of a thread, wrap it in a thread
addThreadBlank : ID -> Expr -> Expr -> Expr
addThreadBlank id blank expr =
  let atb = addThreadBlank id blank in
  if id == B.toID expr
  then
    case expr of
      F tid (Thread exprs) ->
        F tid (Thread (exprs @ [blank]))
      _ ->
        B.newF (Thread [expr, blank])
  else
    case expr of
      F tid (Thread exprs) ->
        let replaced = extendThreadChild id blank exprs in
        if replaced == exprs
        then traverse atb expr
        else F tid (Thread replaced)

      _ -> traverse atb expr

addLambdaBlank : ID -> Expr -> Expr
addLambdaBlank id expr =
  case parentOf_ id expr of
    Just (F lid (Lambda vars body)) as old ->
      let r =
            F lid (Lambda (vars @ [B.new ()]) body)
      in
          replace
            (old |> deMaybe "impossible" |> PExpr)
            (PExpr r)
            expr
    _ -> expr

addObjectLiteralBlanks : ID -> Expr -> (ID, ID, Expr)
addObjectLiteralBlanks id expr =
  case findExn id expr of
    PKey key ->
      case parentOf id expr of
        F olid (ObjectLiteral pairs) as old ->
          let newKey = B.new ()
              newExpr = B.new ()
              newPairs = pairs @ [(newKey, newExpr)]
              new = F olid (ObjectLiteral newPairs)
              replacement = replace
                              (PExpr old)
                              (PExpr new)
                              expr
          in
          (B.toID newKey, B.toID newExpr, replacement)
        _ -> impossible ("key parent must be object", id, expr)
    _ -> impossible ("must add to key", id, expr)


-- Extend the object literal automatically, only if it's the last key in
-- the object.
maybeExtendObjectLiteralAt : PointerData -> Expr -> Expr
maybeExtendObjectLiteralAt pd expr =
  let id = P.toID pd in
  case pd of
    PKey key ->
      case parentOf id expr of
        F olid (ObjectLiteral pairs) ->
          if pairs
             |> LE.last
             |> Maybe.map Tuple.first
             |> (==) (Just key)
          then
            let (_, _, replacement) = addObjectLiteralBlanks id expr in
            replacement
          else expr
        _ -> expr
    _ -> expr

addListLiteralBlanks : ID -> Expr -> Expr
addListLiteralBlanks id expr =
  let new1 = B.new ()
      new2 = B.new ()
      parent = parentOf id expr
  in
  case parent of
    F lid (ListLiteral exprs) ->
      let newExprs = exprs
                     |> List.reverse
                     |> LE.dropWhile B.isBlank
                     |> (@) [new1]
                     |> List.reverse
      in
      replace
        (PExpr parent)
        (PExpr (F lid (ListLiteral newExprs)))
        expr
    _ -> expr



-- Extend the object literal automatically, only if it's the last key in
-- the object.
maybeExtendListLiteralAt : PointerData -> Expr -> Expr
maybeExtendListLiteralAt pd expr =
  let id = P.toID pd in
  case parentOf_ id expr of
    Just (F lid (ListLiteral exprs)) ->
      if exprs
         |> List.filter B.isBlank
         |> List.length
         |> \l -> l <= 1
      then
        let replacement = addListLiteralBlanks id expr in
        replacement
      else expr
    _ -> expr



-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> Expr -> Expr
wrapInThread id expr =
  if B.toID expr == id
  then
    case expr of
      F _ (Thread _) -> expr
      F _ _ -> B.newF (Thread [expr, B.new ()])
      Blank _ -> B.newF (Thread [expr])
      -- decide based on the displayed value, so flatten
  else
    traverse (wrapInThread id) expr

-- Is PointerData a blank inside a thread
isThreadBlank : Expr -> ID -> Bool
isThreadBlank expr p =
  expr |> listThreadBlanks |> List.member p

grandparentIsThread : Expr -> Maybe Expr -> Bool
grandparentIsThread expr parent =
  parent
  |> Maybe.map
       (\p ->
         case parentOf_ (B.toID p) expr of
           Just (F _ (Thread ts)) ->
             ts
             |> List.head
             |> Maybe.map ((/=) p)
             |> Maybe.withDefault True
           _ -> False)
  |> Maybe.withDefault False

getParamIndex : Expr -> ID -> Maybe (String, Int)
getParamIndex expr id =
  let parent = parentOf_ id expr
      inThread = grandparentIsThread expr parent
  in
  case parent of
    Just (F _ (FnCall name args _)) ->
      args
      |> LE.findIndex (\a -> B.toID a == id)
      |> Maybe.map
           (\i -> if inThread
                  then (name, i + 1)
                  else (name, i))
    _ -> Nothing

threadPrevious : ID -> Expr -> Maybe Expr
threadPrevious id ast =
  let parent = parentOf_ id  ast in
  case parent of
    Just (F _ (Thread exprs)) ->
      exprs
      |> List.filter (\e -> B.toID e == id)
      |> List.head
      |> Maybe.andThen (\this -> Util.listPrevious this exprs)
    _ -> Nothing



uses : VarName -> Expr -> List Expr
uses var expr =
  let is_rebinding newbind =
        case newbind of
          Blank _ -> False
          F _ potential ->
            if potential == var then True else False
      u = uses var
  in
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value _ -> []
        Variable potential ->
          if potential == var then [expr] else []
        Let lhs rhs body ->
          if is_rebinding lhs then [] else List.concat [u rhs, u body]
        If cond ifbody elsebody ->
          List.concat [u cond, u ifbody, u elsebody]
        FnCall name exprs _ ->
          exprs |> List.map u |> List.concat
        Lambda vars lexpr ->
          if List.any is_rebinding vars
          then []
          else u lexpr
        Thread exprs ->
          exprs |> List.map u |> List.concat
        FieldAccess obj field ->
          u obj
        ListLiteral exprs ->
          exprs |> List.map u |> List.concat
        ObjectLiteral pairs ->
          pairs |> List.map Tuple.second |> List.map u |> List.concat
        FeatureFlag msg cond a b ->
          List.concat [u cond, u a, u b]


allCallsToFn : String -> Expr -> List Expr
allCallsToFn s e =
  e
  |> allData
  |> List.filterMap
    (\pd ->
      case pd of
        PExpr (F id (FnCall name params r)) ->
          if name == s
          then Just (F id (FnCall name params r))
          else Nothing
        _ -> Nothing)

usesRail : Expr -> Bool
usesRail ast =
  List.any
    (\e ->
        case e of
          PExpr (F _ (FnCall _ _ Rail)) -> True
          _ -> False)
    (allData ast)

-------------------------
-- Ancestors
-------------------------
ancestors : ID -> Expr -> List Expr
ancestors id expr =
  let rec_ancestors : ID -> List Expr -> Expr -> List Expr
      rec_ancestors tofind walk exp =
        let rec id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_)
            reclist id_ e_ walk_ exprs =
              exprs |> List.map (rec id_ e_ walk_) |> List.concat
        in
        if B.toID exp == tofind
        then walk
        else
          case exp of
            Blank _ -> []
            -- no idea what to do here
            F i nexpr ->
              case nexpr of
                Value _ -> []
                Variable _ -> []
                Let lhs rhs body ->
                  reclist id exp walk [rhs, body]
                If cond ifbody elsebody ->
                  reclist id exp walk [cond, ifbody, elsebody]
                FnCall name exprs _ ->
                  reclist id exp walk exprs
                Lambda vars lexpr ->
                  rec id exp walk lexpr
                Thread exprs ->
                  reclist id exp walk exprs
                FieldAccess obj field ->
                  rec id exp walk obj
                ListLiteral exprs ->
                  reclist id expr walk exprs
                ObjectLiteral pairs ->
                  pairs
                  |> List.map Tuple.second
                  |> reclist id expr walk
                FeatureFlag msg cond a b ->
                  reclist id exp walk [cond, a, b]


  in rec_ancestors id [] expr


ancestorsWhere : ID -> Expr -> (Expr -> Bool) -> List Expr
ancestorsWhere id expr fn =
  List.filter fn (ancestors id expr)

threadAncestors : ID -> Expr -> List Expr
threadAncestors id expr =
  ancestorsWhere id expr
    (\e ->
      case e of
        F _ (Thread _) -> True
        _ -> False)


-- includes self
siblings : PointerData -> Expr -> List PointerData
siblings p expr =
  case parentOf_ (P.toID p) expr of
    Nothing -> [p]
    Just parent ->
      case parent of
        F _ (If cond ifbody elsebody) ->
          List.map PExpr [cond, ifbody, elsebody]

        F _ (Let lhs rhs body) ->
          [ PVarBind lhs, PExpr rhs, PExpr body]

        F _ (FnCall name exprs _) ->
          List.map PExpr exprs

        F _ (Lambda vars lexpr) ->
          (List.map PVarBind vars) @ [PExpr lexpr]

        F _ (Thread exprs) ->
          List.map PExpr exprs

        F _ (FieldAccess obj field) ->
          [PExpr obj, PField field]

        F _ (Value _) -> [p]
        F _ (Variable _) -> [p]
        F _ (ObjectLiteral pairs) ->
          pairs
          |> List.map (\(k,v) -> [PKey k, PExpr v])
          |> List.concat
        F _ (ListLiteral exprs) -> List.map PExpr exprs
        F _ (FeatureFlag msg cond a b) ->
          [PFFMsg msg] @ List.map PExpr [cond, a, b]
        Blank _ -> [p]

getValueParent : PointerData -> Expr -> Maybe PointerData
getValueParent p expr =
  let parent = parentOf_ (P.toID p) expr in
  case (P.typeOf p, parent) of
    (Expr, Just (F _ (Thread exprs))) ->
      exprs
      |> List.map PExpr
      |> Util.listPrevious p

    (Field, Just (F _ (FieldAccess obj _))) ->
      Just <| PExpr obj

    _ -> Nothing



--------------------------------
-- PointerData
--------------------------------

allData : Expr -> List PointerData
allData expr =
  let e2ld e = PExpr e
      rl : List Expr -> List PointerData
      rl exprs =
        exprs
        |> List.map allData
        |> List.concat
  in
  [e2ld expr] @
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value v -> []
        Variable name -> []

        Let lhs rhs body ->
          [PVarBind lhs] @ rl [rhs, body]

        If cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        FnCall name exprs _ ->
          rl exprs

        Lambda vars body ->
          (List.map PVarBind vars) @ allData body

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
          allData obj @ [PField field]

        ListLiteral exprs ->
          rl exprs

        ObjectLiteral pairs ->
          pairs
          |> List.map (\(k,v) -> PKey k :: allData v)
          |> List.concat

        FeatureFlag msg cond a b ->
          [PFFMsg msg] @ rl [cond, a, b]

findExn : ID -> Expr -> PointerData
findExn id expr =
  expr
  |> find id
  |> deMaybe "findExn"

find : ID -> Expr -> Maybe PointerData
find id expr =
  expr
  |> allData
  |> List.filter (\d -> id == P.toID d)
  |> assert (\r -> List.length r <= 1) -- guard against dups
  |> List.head


replace : PointerData -> PointerData -> Expr -> Expr
replace search replacement expr =
  replace_ search replacement Nothing expr

within : NExpr -> ID -> Bool
within e id =
  e
  |> F (ID -1)
  |> allData
  |> List.map P.toID
  |> List.member id


replace_ : PointerData -> PointerData -> Maybe Expr -> Expr -> Expr
replace_ search replacement parent expr =
  let r = replace_ search replacement (Just expr) -- expr is new parent
      sId = P.toID search
  in
  if B.toID expr == sId
  then
    case replacement of
      PExpr e ->
        let repl_ =
              case parent of
                -- if pasting it into a thread, make the shape fit
                Just (F _ (Thread (first :: _))) ->
                  case e of
                    F id (FnCall fn (_ :: rest as args) r_) ->
                      if B.toID first == sId
                      then (F id (FnCall fn args r_))
                      else (F id (FnCall fn rest r_))
                    _ -> e
                _ -> e
        in B.replace sId repl_ expr
      _ -> recoverable ("cannot occur", replacement) expr
  else
    case (expr, replacement) of
      (F id (FeatureFlag msg cond a b), PFFMsg newMsg) ->
        if B.toID msg == sId
        then F id (FeatureFlag newMsg cond a b)
        else traverse r expr
      (F id (Let lhs rhs body), PVarBind replacement_) ->
        if B.toID lhs == sId
        then
          let replacementContent =
                case replacement_ of
                  Blank _ -> Nothing
                  F _ var -> Just var
              orig =
                case lhs of
                  Blank _ -> Nothing
                  F _ var -> Just var
              newBody =
                let usesOf =
                      case orig of
                        Just var -> uses var body |> List.map PExpr
                        _ -> []
                    transformUse replacementContent_ old =
                      case old of
                        PExpr (F _ _) ->
                          PExpr (F (gid ()) (Variable replacementContent_))
                        _ -> impossible old
                in
                case (orig, replacementContent) of
                  (Just o, Just r_) ->
                    List.foldr
                      (\use acc ->
                        replace_ use (transformUse r_ use) (Just expr) acc)
                      body
                      usesOf
                  _ -> body
          in
              F id (Let (B.replace sId replacement_ lhs) rhs newBody)
        else traverse r expr
      (F id (Lambda vars body), PVarBind replacement_) ->
        case LE.findIndex (\v -> B.toID v == sId) vars of
          Nothing -> traverse r expr
          Just i ->
            let replacementContent =
                    case replacement_ of
                      Blank _ -> Nothing
                      F _ var -> Just var
                orig =
                  case LE.getAt i vars |> deMaybe "we somehow lost it?" of
                    Blank _ -> Nothing
                    F _ var -> Just var
                newBody =
                  let usesInBody =
                        case orig of
                          Just v ->
                            uses v body |> List.map PExpr
                          Nothing -> []
                      transformUse replacementContent_ old =
                        case old of
                          PExpr (F _ _) ->
                            PExpr (F (gid ()) (Variable replacementContent_))
                          _ -> impossible old
                  in
                  case (orig, replacementContent) of
                    (Just o, Just r_) ->
                      List.foldr
                        (\use acc ->
                          replace_ use (transformUse r_ use) (Just expr) acc)
                        body
                        usesInBody
                    _ -> body
                newVars =
                  LE.updateAt i (\old  -> B.replace sId replacement_ old) vars
            in
                F id (Lambda newVars newBody)

      (F id (FieldAccess obj field), PField replacement_) ->
        if B.toID field == sId
        then F id (FieldAccess obj (B.replace sId replacement_ field))
        else traverse r expr

      (F id (ObjectLiteral pairs), PKey replacement_) ->
        pairs
        |> List.map (\(k,v) ->
          let newK =
                if B.toID k == sId
                then replacement_
                else k
          in
              (newK, r v))
        |> ObjectLiteral
        |> F id

      _ -> traverse r expr


deleteExpr : PointerData -> Expr -> ID -> Expr
deleteExpr p expr id =
  let replacement = P.emptyD_ id (P.typeOf p)
  in replace p replacement expr

clone : Expr -> Expr
clone expr =
  let nid = gid ()
      c be = clone be
      cl bes = List.map c bes
      cString : BlankOr String -> BlankOr String
      cString = B.clone identity
      cNExpr nexpr =
        case nexpr of
          Let lhs rhs body -> Let (cString lhs) (c rhs) (c body)
          If cond ifbody elsebody -> If (c cond) (c ifbody) (c elsebody)
          FnCall name exprs r -> FnCall name (cl exprs) r
          Lambda vars body -> Lambda (List.map cString vars) (c body)
          Thread exprs -> Thread (cl exprs)
          FieldAccess obj field -> FieldAccess (c obj) (cString field)
          Value v -> Value v
          Variable name -> Variable name
          ListLiteral exprs -> ListLiteral (cl exprs)
          ObjectLiteral pairs ->
            ObjectLiteral (List.map (\(k,v) -> (cString k, c v)) pairs)
          FeatureFlag msg cond a b ->
            FeatureFlag (cString msg) (c cond) (c a) (c b)
  in B.clone cNExpr expr

isDefinitionOf : VarName -> Expr -> Bool
isDefinitionOf var exp =
  case exp of
    Blank _ -> False
    F id e ->
      case e of
        Let b _ _ ->
          case b of
            Blank _ -> False
            F _ vb ->
              vb == var
        Lambda vars _ ->
          vars
          |> List.any
            (\v ->
              case v of
                Blank _ -> False
                F _ vb ->
                  vb == var)
        _ -> False

freeVariables : Expr -> List (ID, VarName)
freeVariables ast =
  let definedAndUsed = ast
                       |> allData
                       |> List.filterMap
                         (\n ->
                           case n of
                             PExpr boe ->
                               case boe of
                                 Blank _ -> Nothing
                                 F id e as expr ->
                                   case e of
                                     Let (F _ lhs) rhs body->
                                       Just (uses lhs body)
                                     Lambda vars body ->
                                       vars
                                       |> List.filterMap B.toMaybe
                                       |> List.map (\v -> uses v body)
                                       |> List.concat
                                       |> Just
                                     _ -> Nothing
                             _ -> Nothing)
                       |> List.concat
                       |> List.map (B.toID >> deID)
                       |> Set.fromList
   in
      ast
      |> allData
      |> List.filterMap
        (\n ->
          case n of
            PExpr boe ->
              case boe of
                Blank _ -> Nothing
                F id e ->
                  case e of
                    Variable name ->
                      if Set.member (deID id) definedAndUsed
                      then Nothing
                      else Just (id, name)
                    _ -> Nothing
            _ -> Nothing)
      |> LE.uniqueBy
        (\(_, name) -> name)
