module Flags exposing (..)

type alias Parameter =
  { name: String
  , tipe: String
  , block_args: List String
  , optional: Bool
  , description: String
  }

type alias Function =
  { name: String
  , parameters: List Parameter
  , description: String
  , return_type: String
  , preview_execution_safe: Bool
  , deprecated: Bool
  , infix: Bool
  }

type alias Flags =
  { editorState: Maybe String
  , complete: List Function
  , userContentHost : String
  , environment: String
  , csrfToken : String
  }


