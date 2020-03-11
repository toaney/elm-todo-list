module Main exposing (main)

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type alias Task =
    { description : String
    }


type alias Model =
    { newTaskDescription : String
    , tasks : List Task
    }


init : Model
init =
    { newTaskDescription = ""
    , tasks =
        [ { description = "clean room" }
        , { description = "buy groceries" }
        ]
    }



-- UPDATE


type Msg
    = UserClickedAddTask


update msg model =
    { model | tasks = { description = "New Task" } :: model.tasks }



-- VIEW


view model =
    Html.div []
        (newTaskView
            :: List.map taskView model.tasks
        )


newTaskView =
    Html.div []
        [ Html.input [ HA.placeholder "type here" ] []
        , Html.button [ HE.onClick UserClickedAddTask ] [ Html.text "add" ]
        ]


taskView task =
    Html.div [] [ Html.text task.description ]


main =
    Browser.sandbox { init = init, update = update, view = view }
