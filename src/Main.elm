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
    | UserEditedNewTaskDescription String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedAddTask ->
            { model
                | tasks = { description = model.newTaskDescription } :: model.tasks
                , newTaskDescription = ""
            }

        UserEditedNewTaskDescription newDescription ->
            { model | newTaskDescription = newDescription }



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        (newTaskView model
            :: List.map taskView model.tasks
        )


newTaskView : Model -> Html.Html Msg
newTaskView model =
    Html.div []
        [ Html.input [ HA.placeholder "type here", HA.value model.newTaskDescription, HE.onInput UserEditedNewTaskDescription ] []
        , Html.button [ HE.onClick UserClickedAddTask ] [ Html.text "add" ]
        ]


taskView : Task -> Html.Html Msg
taskView task =
    Html.div [] [ Html.text task.description ]


main =
    Browser.sandbox { init = init, update = update, view = view }
