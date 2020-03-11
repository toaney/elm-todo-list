module Main exposing (Task, deleteTask, main)

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type alias Task =
    { description : String
    , id : Int
    }


type alias Model =
    { newTaskDescription : String
    , tasks : List Task
    , nextTaskId : Int
    }


init : Model
init =
    { newTaskDescription = ""
    , tasks =
        [ { description = "clean room", id = 1 }
        , { description = "buy groceries", id = 2 }
        ]
    , nextTaskId = 3
    }



-- UPDATE


type Msg
    = UserClickedAddTask
    | UserEditedNewTaskDescription String
    | UserClickedDeleteTask Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedAddTask ->
            { model
                | tasks = { id = model.nextTaskId, description = model.newTaskDescription } :: model.tasks
                , newTaskDescription = ""
                , nextTaskId = model.nextTaskId + 1
            }

        UserEditedNewTaskDescription newDescription ->
            { model | newTaskDescription = newDescription }

        UserClickedDeleteTask taskId ->
            { model
                | tasks = deleteTask taskId model.tasks
            }


deleteTask : Int -> List Task -> List Task
deleteTask id tasks =
    List.filter (\task -> task.id /= id) tasks



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
    Html.div []
        [ Html.text task.description
        , Html.button [ HE.onClick (UserClickedDeleteTask task.id) ] [ Html.text "delete" ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
