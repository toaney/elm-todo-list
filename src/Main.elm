module Main exposing (main)

import Browser
import Html



-- MODEL


type alias Task =
    { name : String
    }


type alias Model =
    List Task


init : Model
init =
    []



-- UPDATE


type Msg
    = Noop


update msg model =
    model



-- VIEW


view model =
    Html.text "Hello World! "


main =
    Browser.sandbox { init = init, update = update, view = view }
