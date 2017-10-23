module Tests exposing (all)

-- tests
import ElmTest.Extra exposing (Test, describe)

-- builtins

-- libs

-- dark
import TestEntryParser
import TestLayout
import TestAutocomplete

all : Test
all =
  describe "All tests"
    [ TestEntryParser.all
    , TestAutocomplete.all
    , TestLayout.all
    ]



