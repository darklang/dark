module Clipboard exposing (..)


-- Dark
import AST
import Entry
import Pointer as P
import Prelude exposing (..)
import Random
import Toplevel as TL
import Types exposing (..)
import Util
import Random.Steps

copy : Model -> Toplevel -> (Maybe PointerData) -> Modification
copy m tl mp =
  case tl.data of
    TLDB _ ->
      NoChange
    TLHandler h ->
      case mp of
        Nothing -> CopyToClipboard (Just <| PExpr h.ast)
        Just p -> CopyToClipboard (TL.find tl (P.toID p))
    TLFunc f ->
      case mp of
        Nothing -> CopyToClipboard (Just <| PExpr f.ast)
        Just p -> CopyToClipboard (TL.find tl (P.toID p))

cut : Model -> Toplevel -> PointerData -> Modification
cut m tl p =
  let pid = P.toID p
      pred = TL.getPrevBlank tl (Just p)
             |> Maybe.map P.toID
  in
      case tl.data of
        TLDB _ ->
          NoChange
        TLHandler h ->
          let (randomInt, nextSeed) = Random.step Util.randomInt m.seed
              newClipboard = TL.find tl pid
              newH = ID randomInt
                    |> TL.delete tl p
                    |> TL.asHandler
                    |> deMaybe "cut"

          in Many [ CopyToClipboard newClipboard
                  , RPC ( [ SetHandler tl.id tl.pos newH ]
                          , FocusNext tl.id pred)
                  , SetSeed nextSeed
                  ]
        TLFunc f ->
          let (randomInt, nextSeed) = Random.step Util.randomInt m.seed
              newClipboard = TL.find tl pid
              newF = ID randomInt
                    |> TL.delete tl p
                    |> TL.asUserFunction
                    |> deMaybe "cut"

          in Many [ CopyToClipboard newClipboard
                  , RPC ( [ SetFunction newF ]
                          , FocusNext tl.id pred)
                  , SetSeed nextSeed
                  ]

paste : Model -> Toplevel -> ID -> Modification
paste m tl id =
  case m.clipboard of
    Nothing -> NoChange
    Just pd ->
      let cloned = TL.clonePointerData pd in
      case tl.data of
        TLDB _ ->
          NoChange
        TLHandler h ->
          let newAst = AST.replace (TL.findExn tl id) cloned h.ast in
          if newAst == h.ast
          then NoChange -- paste doesn't always make sense
          else
            RPC ( [ SetHandler tl.id tl.pos { h | ast = newAst } ]
            , FocusExact tl.id (P.toID cloned))
        TLFunc f ->
          let newAst = AST.replace (TL.findExn tl id) cloned f.ast in
          if newAst == f.ast
          then NoChange -- paste doesn't always make sense
          else
            RPC ( [ SetFunction { f | ast = newAst } ]
            , FocusExact tl.id (P.toID cloned))

peek : Model -> Clipboard
peek m =
  Maybe.map TL.clonePointerData m.clipboard

newFromClipboard : Model -> Pos -> Modification
newFromClipboard m pos =
  let (randomInts, nextSeed) = Random.Steps.two Util.randomInt m.seed
      nid = TLID randomInts.first
      ast =
        case peek m of
          Nothing -> Blank (ID randomInts.second)
          Just a ->
            case a of
              PExpr exp -> exp
              _ -> Blank (ID randomInts.second)
      spec = Entry.newHandlerSpec ()
      handler = { ast = ast, spec = spec, tlid = nid }
  in
      Many [ RPC ([SetHandler nid pos handler], FocusNext nid Nothing)
           , SetSeed nextSeed
           ]
