module Utils exposing (..)

import Model exposing (..)
import List exposing (foldl)

getName : User -> String
getName u = u.username

showNames : Maybe (List User) -> String
showNames user =
  case user of
    Nothing ->
      "No users"

    Just u ->
      foldl ( getName >> (++) ) "" u