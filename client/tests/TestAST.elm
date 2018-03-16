module TestAST exposing (all)

-- tests
import Test exposing (..)
import Expect exposing (Expectation)

-- builtins

-- libs

-- dark
import Types exposing (..)
import AST
import Blank

id1 = ID 5
id2 = ID 10

all : Test
all =
  describe "ast"
    [ test "isThreadBlank for thread" <|
      \_ -> Expect.true "false"
      (AST.isThreadBlank 
        (F id1 (Thread [Blank id2]))
        (PFilled Expr id2))

    ,  test "isThreadBlank for blank" <|
      \_ -> Expect.false "true"
      (AST.isThreadBlank
        (Blank id1)
        (PFilled Expr id1))

    ,  test "isThreadBlank for thread non-blank" <|
      \_ -> Expect.false "true"
      (AST.isThreadBlank 
        (F id1 (Thread [F id2 (Value "")]))
        (PFilled Expr id2))
    ]


