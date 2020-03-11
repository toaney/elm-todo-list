module Main exposing (main)

import Browser
import Html
import Html.Attributes as HA



-- MODEL


type alias Task =
    { description : String
    }


type alias Model =
    List Task


init : Model
init =
    [ { description = "clean room" }
    , { description = "buy groceries" }
    ]



-- UPDATE


type Msg
    = Noop


update msg model =
    model



-- VIEW


view model =
    Html.div []
        (newTaskView
            :: List.map taskView model
        )


newTaskView =
    Html.div []
        [ Html.input [ HA.placeholder "type here" ] []
        , Html.button [] [ Html.text "add"]
        ]


taskView task =
    Html.div [] [ Html.text task.description ]


main =
    Browser.sandbox { init = init, update = update, view = view }
