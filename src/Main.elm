module Main exposing (main)

import Browser
import Html



-- MODEL


type alias Model =
    Int


init : Model
init =
    1



-- UPDATE


type Msg
    = Noop


update msg model =
    model



-- VIEW


view model =
    Html.text ("Hello World! " ++ String.fromInt model)


main =
    Browser.sandbox { init = init, update = update, view = view }
