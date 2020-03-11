module Main exposing (Task, deleteTask, main)

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type Id
    = IdValue Int


type TaskName
    = TaskNameValue String


type TaskDescription
    = TaskDescriptionValue String


type alias Task =
    { name : String
    , description : String
    , id : Id
    }


type alias Model =
    { newTaskName : String
    , newTaskDescription : String
    , tasks : List Task
    , nextTaskId : Id
    }


init : Model
init =
    { newTaskName = ""
    , newTaskDescription = ""
    , tasks =
        []
    , nextTaskId = IdValue 1
    }
        |> addTask (TaskNameValue "clean room") (TaskDescriptionValue "make bed and vacuum")
        |> addTask (TaskNameValue "buy groceries") (TaskDescriptionValue "milk, eggs, juice")



-- UPDATE


type Msg
    = UserClickedAddTask
    | UserEditedNewTaskName String
    | UserEditedNewTaskDescription String
    | UserClickedDeleteTask Id


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedAddTask ->
            -- {
            --     newTaskDescription = ""
            --     , nextTaskId = model.nextTaskId
            --     , tasks = model.tasks
            -- }
            { model
                | newTaskDescription = ""
                , newTaskName = ""
            }
                |> addTask (TaskNameValue model.newTaskName) (TaskDescriptionValue model.newTaskDescription)

        UserEditedNewTaskName newName ->
            { model | newTaskName = newName }

        UserEditedNewTaskDescription newDescription ->
            { model | newTaskDescription = newDescription }

        UserClickedDeleteTask taskId ->
            { model
                | tasks = deleteTask taskId model.tasks
            }


deleteTask : Id -> List Task -> List Task
deleteTask id tasks =
    List.filter (\task -> task.id /= id) tasks


addTask : TaskName -> TaskDescription -> Model -> Model
addTask name description model =
    { model
        | tasks = { name = nameValue name, id = model.nextTaskId, description = descriptionValue description } :: model.tasks
        , nextTaskId = incrementId model.nextTaskId
    }


incrementId : Id -> Id
incrementId id =
    mapId ((+) 1) id


mapId : (Int -> Int) -> Id -> Id
mapId f (IdValue id) =
    IdValue (f id)


nameValue : TaskName -> String
nameValue (TaskNameValue name) =
    name


descriptionValue : TaskDescription -> String
descriptionValue (TaskDescriptionValue description) =
    description



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
        [ Html.input [ HA.placeholder "Name", HA.value model.newTaskName, HE.onInput UserEditedNewTaskName ] []
        , Html.input [ HA.placeholder "Description", HA.value model.newTaskDescription, HE.onInput UserEditedNewTaskDescription ] []
        , Html.button [ HE.onClick UserClickedAddTask ] [ Html.text "add" ]
        ]


taskView : Task -> Html.Html Msg
taskView task =
    Html.div []
        [ Html.text task.name
        , Html.text " : "
        , Html.text task.description
        , Html.button [ HE.onClick (UserClickedDeleteTask task.id) ] [ Html.text "delete" ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
