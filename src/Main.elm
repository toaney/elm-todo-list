module Main exposing (EditableName(..), Id(..), Task, TaskDescription(..), TaskName(..), TaskViewState(..), deleteTask, main, toggleTaskViewState)

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type Id
    = Id Int


type TaskName
    = TaskName String


type TaskDescription
    = TaskDescription String


type alias Task =
    { editableName : EditableName
    , description : TaskDescription
    , id : Id
    , viewState : TaskViewState
    , tempName : TaskName
    }


type EditableName
    = NotEditingName TaskName
    | EditingName TaskName


type TaskViewState
    = Expanded
    | Collapsed


type alias Model =
    { newTaskName : TaskName
    , newTaskDescription : TaskDescription
    , tasks : List Task
    , nextTaskId : Id
    }


init : Model
init =
    { newTaskName = TaskName ""
    , newTaskDescription = TaskDescription ""
    , tasks =
        []
    , nextTaskId = Id 1
    }
        |> addTask (TaskName "clean room") (TaskDescription "make bed and vacuum")
        |> addTask (TaskName "buy groceries") (TaskDescription "milk, eggs, juice")



-- UPDATE


type Msg
    = UserClickedAddTask
    | UserEditedNewTaskName TaskName
    | UserEditedNewTaskDescription TaskDescription
    | UserClickedDeleteTask Id
    | UserClickedToggleTaskViewState Id
    | UserClickedEditTaskName Id
    | UserClickedSaveEditTaskName Id
    | UserClickedCancelEditTaskName Id
    | UserEditedTaskName Id TaskName


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
                | newTaskDescription = TaskDescription ""
                , newTaskName = TaskName ""
            }
                |> addTask model.newTaskName model.newTaskDescription

        UserEditedNewTaskName newName ->
            { model | newTaskName = newName }

        UserEditedNewTaskDescription newDescription ->
            { model | newTaskDescription = newDescription }

        UserClickedDeleteTask taskId ->
            { model
                | tasks = deleteTask taskId model.tasks
            }

        UserClickedToggleTaskViewState taskId ->
            { model
                | tasks = toggleTaskViewState taskId model.tasks
            }

        UserClickedEditTaskName taskId ->
            { model
                | tasks = startEditingTaskName taskId model.tasks
            }

        UserClickedSaveEditTaskName taskId ->
            { model
                | tasks = updateName taskId model.tasks |> stopEditingTaskName taskId
            }

        UserClickedCancelEditTaskName taskId ->
            { model
                | tasks = stopEditingTaskName taskId model.tasks
            }

        UserEditedTaskName taskId editedTaskName ->
            { model
                | tasks = updateTempName taskId editedTaskName model.tasks
            }


deleteTask : Id -> List Task -> List Task
deleteTask id tasks =
    List.filter (\task -> task.id /= id) tasks



-- deleteTask : Id -> List Task -> List Task
-- deleteTask id =
--     List.filter (\task -> task.id /= id)


editTaskForId : Id -> (Task -> Task) -> List Task -> List Task
editTaskForId id editFunction tasks =
    let
        editTaskIfId : Task -> Task
        editTaskIfId task =
            if id == task.id then
                editFunction task

            else
                task
    in
    List.map editTaskIfId tasks


updateName : Id -> List Task -> List Task
updateName id tasks =
    let
        setEditableName editableName newTaskName =
            case editableName of
                EditingName _ ->
                    EditingName newTaskName

                NotEditingName _ ->
                    NotEditingName newTaskName

        updateTaskName task =
            { task
                | editableName = setEditableName task.editableName task.tempName
                , tempName = task.tempName
            }
    in
    editTaskForId id updateTaskName tasks


updateTempName : Id -> TaskName -> List Task -> List Task
updateTempName id temporaryName tasks =
    let
        newTempName task =
            { task
                | tempName = temporaryName
            }
    in
    editTaskForId id newTempName tasks


stopEditingTaskName : Id -> List Task -> List Task
stopEditingTaskName id tasks =
    let
        stopEditing : Task -> Task
        stopEditing task =
            case task.editableName of
                EditingName taskName ->
                    { task
                        | editableName = NotEditingName taskName
                        , tempName = taskName
                    }

                NotEditingName _ ->
                    task
    in
    editTaskForId id stopEditing tasks


startEditingTaskName : Id -> List Task -> List Task
startEditingTaskName id tasks =
    let
        startEditing : Task -> Task
        startEditing task =
            case task.editableName of
                EditingName _ ->
                    task

                NotEditingName taskName ->
                    { task
                        | editableName = EditingName taskName
                    }
    in
    editTaskForId id startEditing tasks


toggleTaskViewState : Id -> List Task -> List Task
toggleTaskViewState id tasks =
    let
        toggleViewState task =
            case task.viewState of
                Expanded ->
                    { task
                        | viewState = Collapsed
                    }

                Collapsed ->
                    { task
                        | viewState = Expanded
                    }
    in
    editTaskForId id toggleViewState tasks


addTask : TaskName -> TaskDescription -> Model -> Model
addTask name description model =
    { model
        | tasks = { editableName = NotEditingName name, id = model.nextTaskId, description = description, viewState = Collapsed, tempName = name } :: model.tasks
        , nextTaskId = incrementId model.nextTaskId
    }


incrementId : Id -> Id
incrementId id =
    mapId ((+) 1) id


mapId : (Int -> Int) -> Id -> Id
mapId f (Id id) =
    Id (f id)


nameValue : TaskName -> String
nameValue (TaskName name) =
    name


descriptionValue : TaskDescription -> String
descriptionValue (TaskDescription description) =
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
        [ Html.input
            [ HA.placeholder "Name"
            , HA.value <| nameValue model.newTaskName
            , HE.onInput <| TaskName >> UserEditedNewTaskName
            ]
            []
        , Html.input
            [ HA.placeholder "Description"
            , HA.value <| descriptionValue model.newTaskDescription
            , HE.onInput (\taskName -> TaskDescription taskName |> UserEditedNewTaskDescription)
            ]
            []
        , Html.button [ HE.onClick UserClickedAddTask ] [ Html.text "add" ]
        ]


taskView : Task -> Html.Html Msg
taskView task =
    let
        deleteButtonView =
            Html.button [ HE.onClick (UserClickedDeleteTask task.id) ] [ Html.text "delete" ]
    in
    case task.viewState of
        Collapsed ->
            Html.div []
                [ taskNameView task
                , deleteButtonView
                ]

        Expanded ->
            Html.div []
                [ taskNameView task
                , Html.div [] [ Html.text (descriptionValue task.description) ]
                , deleteButtonView
                ]


taskNameView : Task -> Html.Html Msg
taskNameView task =
    case task.editableName of
        EditingName taskName ->
            Html.div
                []
                -- [ HA.placeholder "Description"
                -- , HA.value <| descriptionValue model.newTaskDescription
                -- , HE.onInput (\taskName -> TaskDescription taskName |> UserEditedNewTaskDescription)
                -- ]
                -- [ HA.placeholder "Name"
                -- , HA.value <| nameValue model.newTaskName
                -- , HE.onInput <| TaskName >> UserEditedNewTaskName
                -- ]
                [ Html.input
                    [ HE.onInput (\name -> TaskName name |> UserEditedTaskName task.id)
                    , HA.value (nameValue task.tempName)
                    ]
                    []
                , Html.button [ HE.onClick (UserClickedSaveEditTaskName task.id) ] [ Html.text "save" ]
                , Html.button [ HE.onClick (UserClickedCancelEditTaskName task.id) ] [ Html.text "cancel" ]
                ]

        NotEditingName taskName ->
            Html.div
                []
                [ Html.span [ HE.onClick <| UserClickedToggleTaskViewState task.id ] [ Html.text (nameValue taskName) ]
                , Html.button [ HE.onClick (UserClickedEditTaskName task.id) ] [ Html.text "edit" ]
                ]


main =
    Browser.sandbox { init = init, update = update, view = view }
