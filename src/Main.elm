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
    [ { name = "clean room" }
    , { name = "buy groceries" }
    ]



-- UPDATE


type Msg
    = Noop


update msg model =
    model



-- VIEW


view model =
    Html.div [] (List.map taskView model)


taskView task =
    Html.div [] [ Html.text task.name ]


main =
    Browser.sandbox { init = init, update = update, view = view }
