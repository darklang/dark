module Commands exposing (..)

-- builtins

-- libs

-- dark
-- import Prelude exposing (..)
import Types exposing (..)
import Refactor
import Toplevel as TL

executeCommand : Model -> TLID -> ID -> Maybe AutocompleteItem -> Modification
executeCommand m tlid id highlighted =
  case highlighted of
    Just (ACCommand command) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
      in
      command.action m tl pd
    _ -> NoChange

endCommandExecution : Model -> TLID -> ID -> Modification
endCommandExecution m tlid id =
  Many
    [ AutocompleteMod ACReset
    , Select tlid (Just id)
    ]


commands : List Command
commands =
  [ { commandName = "extract-function"
    , action = Refactor.extractFunction
    , doc = "Extract expression into a function"
    , shortcut = "Ctrl-F"
    }
  , { commandName = "extract-variable"
    , action = Refactor.extractVariable
    , doc = "Extract expression into a variable"
    , shortcut = "Ctrl-Shift-L"
    }
  , { commandName = "wrap-if-condition"
    , action = Refactor.wrap Refactor.WIfCond
    , doc = "Wrap the expression in an if, using the expression as the condition"
    , shortcut = "Ctrl-Alt-C"
    }
  , { commandName = "wrap-if-then"
    , action = Refactor.wrap Refactor.WIfThen
    , doc = "Wrap the expression in an if, putting this expression in the `then` body"
    , shortcut = "Ctrl-I"
    }
  , { commandName = "wrap-if-else"
    , action = Refactor.wrap Refactor.WIfElse
    , doc = "Wrap the expression in an if, putting this expression in the `else` body"
    , shortcut = "Ctrl-Alt-I"
    }
  , { commandName = "insert-let-above"
    , action = Refactor.wrap Refactor.WLetBody
    , doc = "Add a let on the line above"
    , shortcut = "Ctrl-B"
    }
  , { commandName = "insert-let-here"
    , action = Refactor.wrap Refactor.WLetRHS
    , doc = "Wrap expression in a let"
    , shortcut = "Ctrl-L"
    }
  , { commandName = "toggle-expression-on-rail"
    , action = Refactor.toggleOnRail
    , doc = "Switch between using the error rail, or handling this expression yourself"
    , shortcut = "Alt-E"
    }

  ]
