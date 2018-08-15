module Commands exposing (..)

-- builtins

-- libs

-- dark
-- import Prelude exposing (..)
import Types exposing (..)

executeCommand : Model -> TLID -> ID -> Maybe AutocompleteItem -> Modification
executeCommand m tlid id highlighted =
  let successMods =
    case highlighted of
      Just (ACCommand command) ->
        [ Error ("Executing command: " ++ command.name) ]
      _ -> []
  in
      Many (successMods ++ [endCommandExecution m tlid id])

endCommandExecution : Model -> TLID -> ID -> Modification
endCommandExecution m tlid id =
  Many
    [ AutocompleteMod ACReset
    , Select tlid (Just id)
    ]


commands : List Command
commands =
  [ { name = "extract-function"
    , modification = (Error "TODO")
    , doc = "Extract expression into a function"
    , shortcut = "Ctrl-F"
    }
  , { name = "extract-variable"
    , modification = (Error "TODO")
    , doc = "Extract expression into a variable"
    , shortcut = "Ctrl-Shift-L"
    }
  , { name = "wrap-if-condition"
    , modification = (Error "TODO")
    , doc = "Wrap the expression in an if, using the expression as the condition"
    , shortcut = "Ctrl-Alt-C"
    }
  , { name = "wrap-if-then"
    , modification = (Error "TODO")
    , doc = "Wrap the expression in an if, putting this expression in the `then` body"
    , shortcut = "Ctrl-I"
    }
  , { name = "wrap-if-else"
    , modification = (Error "TODO")
    , doc = "Wrap the expression in an if, putting this expression in the `else` body"
    , shortcut = "Ctrl-Alt-I"
    }
  , { name = "insert-let-above"
    , modification = (Error "TODO")
    , doc = "Add a let on the line above"
    , shortcut = "Ctrl-B"
    }
  , { name = "insert-let-here"
    , modification = (Error "TODO")
    , doc = "Wrap expression in a let"
    , shortcut = "Ctrl-L"
    }
  , { name = "toggle-expression-on-rail"
    , modification = (Error "TODO")
    , doc = "Switch between using the error rail, or handling this expression yourself"
    , shortcut = "Alt-E"
    }

  ]
