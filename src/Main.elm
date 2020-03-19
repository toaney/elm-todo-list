module Main exposing (Id(..), Task, TaskDescription(..), TaskName(..), TaskViewState(..), deleteTask, main, toggleTaskViewState)

{-| TODO

1.  ✓ Make task.description editable
2.  ✓ Rename task.editableName to task.name
3.  ✓ Read up on type variables
    <https://riptutorial.com/elm/example/8809/type-variables>
    <https://elmprogramming.com/type-system.html>
4.  ✓ TRY (stretch goal) combining `EditableName` and `EditableDescription` into a single `Editable a`,
5.  ~ TRY Building generic functions for startEditing, stopEditing, cancelEditing, updateBuffer, etc
6.  TRY (stretch goal) Notice that our Model has `newTaskName` `newTaskDescription` fields. Combine these into a single
    `newTask` field of type `Task`.

-}

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
    { name : Editable TaskName
    , description : Editable TaskDescription
    , id : Id
    , viewState : TaskViewState
    }


type Editable a
    = NotEditing a
    | Editing { originalValue : a, buffer : a }



-- type EditableName
--     = NotEditingName TaskName
--     | EditingName { originalValue : TaskName, buffer : TaskName }
-- type EditableDescription
--     = NotEditingDescription TaskDescription
--     | EditingDescription { originalDescription : TaskDescription, descriptionBuffer : TaskDescription }


type TaskEditableField
    = TaskEditableName
    | TaskEditableDescription


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
    | UserClickedEditTaskDescription Id
    | UserClickedSaveEditTaskDescription Id
    | UserClickedCancelEditTaskDescription Id
    | UserEditedTaskDescription Id TaskDescription


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
                | tasks = editTaskForId taskId startEditingName model.tasks
            }

        UserClickedSaveEditTaskName taskId ->
            { model
                | tasks = editTaskForId taskId stopEditingName model.tasks
            }

        UserClickedCancelEditTaskName taskId ->
            { model
                | tasks = editTaskForId taskId cancelEditingName model.tasks
            }

        UserEditedTaskName taskId editedTaskName ->
            { model
                | tasks = updateTaskNameBuffer taskId editedTaskName model.tasks
            }

        UserClickedEditTaskDescription taskId ->
            { model
                | tasks = editTaskForId taskId startEditingDescription model.tasks
            }

        UserClickedSaveEditTaskDescription taskId ->
            { model
                | tasks = editTaskForId taskId stopEditingDescription model.tasks
            }

        UserClickedCancelEditTaskDescription taskId ->
            { model
                | tasks = editTaskForId taskId cancelEditingDescription model.tasks
            }

        UserEditedTaskDescription taskId editedTaskDescription ->
            { model
                | tasks = updateTaskDescriptionBuffer taskId editedTaskDescription model.tasks
            }



-- { model
--     | tasks = updateTaskDescriptionBuffer taskId model.tasks
-- }


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


updateTaskNameBuffer : Id -> TaskName -> List Task -> List Task
updateTaskNameBuffer id tempValue tasks =
    let
        updateBuffer : Editable TaskName -> Editable TaskName
        updateBuffer value =
            case value of
                Editing { originalValue } ->
                    Editing { originalValue = originalValue, buffer = tempValue }

                NotEditing _ ->
                    value

        setNameBuffer : Task -> Task
        setNameBuffer task =
            { task
                | name = updateBuffer task.name
            }
    in
    editTaskForId id setNameBuffer tasks


updateTaskDescriptionBuffer : Id -> TaskDescription -> List Task -> List Task
updateTaskDescriptionBuffer id tempValue tasks =
    let
        updateBuffer : Editable TaskDescription -> Editable TaskDescription
        updateBuffer value =
            case value of
                Editing { originalValue } ->
                    Editing { originalValue = originalValue, buffer = tempValue }

                NotEditing _ ->
                    value

        setDescriptionBuffer : Task -> Task
        setDescriptionBuffer task =
            case task.description of
                Editing { originalValue } ->
                    { task
                        | description = Editing { originalValue = originalValue, buffer = tempValue }
                    }

                NotEditing _ ->
                    { task
                        | description = task.description
                    }
    in
    editTaskForId id setDescriptionBuffer tasks


stopEditing : Editable a -> Editable a
stopEditing value =
    case value of
        Editing { buffer } ->
            NotEditing buffer

        NotEditing _ ->
            value


stopEditingName : Task -> Task
stopEditingName task =
    { task
        | name = stopEditing task.name
    }


stopEditingDescription : Task -> Task
stopEditingDescription task =
    { task
        | description = stopEditing task.description
    }


cancelEditing : Editable a -> Editable a
cancelEditing editable =
    case editable of
        Editing { originalValue, buffer } ->
            NotEditing originalValue

        NotEditing _ ->
            editable


cancelEditingName : Task -> Task
cancelEditingName task =
    { task
        | name = cancelEditing task.name
    }


cancelEditingDescription : Task -> Task
cancelEditingDescription task =
    { task
        | description = cancelEditing task.description
    }



-- cancelEditingTaskName : Id -> List Task -> List Task
-- cancelEditingTaskName id tasks =
--     let
--         cancelEditing : Editable a -> Editable a
--         cancelEditing value =
--             case value of
--                 Editing { originalValue, buffer } ->
--                     NotEditing originalValue
--                 NotEditing _ ->
--                     value
--         cancelEditingName : Task -> Task
--         cancelEditingName task =
--             case task.name of
--                 Editing { originalValue, buffer } ->
--                     { task
--                         | name = NotEditing originalValue
--                     }
--                 NotEditing _ ->
--                     { task
--                         | name = task.name
--                     }
--     in
--     editTaskForId id cancelEditingName tasks
-- cancelEditingTaskDescription : Id -> List Task -> List Task
-- cancelEditingTaskDescription id tasks =
--     let
--         cancelEditingDescription : Task -> Task
--         cancelEditingDescription task =
--             case task.description of
--                 Editing { originalValue, buffer } ->
--                     { task
--                         | description = NotEditing originalValue
--                     }
--                 NotEditing _ ->
--                     { task
--                         | description = task.description
--                     }
--     in
--     editTaskForId id cancelEditingDescription tasks


startEditing : Editable a -> Editable a
startEditing editable =
    case editable of
        Editing _ ->
            editable

        NotEditing value ->
            Editing { originalValue = value, buffer = value }


startEditingName : Task -> Task
startEditingName task =
    { task
        | name = startEditing task.name
    }


startEditingDescription : Task -> Task
startEditingDescription task =
    { task
        | description = startEditing task.description
    }


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
        | tasks = { name = NotEditing name, id = model.nextTaskId, description = NotEditing description, viewState = Collapsed } :: model.tasks
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
                , taskDescriptionView task
                , deleteButtonView
                ]


taskNameView : Task -> Html.Html Msg
taskNameView task =
    case task.name of
        Editing { buffer } ->
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
                    , HA.value (nameValue buffer)
                    ]
                    []
                , Html.button [ HE.onClick (UserClickedSaveEditTaskName task.id) ] [ Html.text "save" ]
                , Html.button [ HE.onClick (UserClickedCancelEditTaskName task.id) ] [ Html.text "cancel" ]
                ]

        NotEditing taskName ->
            Html.div
                []
                [ Html.span [ HE.onClick <| UserClickedToggleTaskViewState task.id ] [ Html.text (nameValue taskName) ]
                , Html.button [ HE.onClick (UserClickedEditTaskName task.id) ] [ Html.text "edit" ]
                ]


taskDescriptionView : Task -> Html.Html Msg
taskDescriptionView task =
    case task.description of
        Editing { buffer } ->
            Html.div
                []
                [ Html.input
                    [ HE.onInput (\description -> TaskDescription description |> UserEditedTaskDescription task.id)
                    , HA.value (descriptionValue buffer)
                    ]
                    []
                , Html.button [ HE.onClick (UserClickedSaveEditTaskDescription task.id) ] [ Html.text "save" ]
                , Html.button [ HE.onClick (UserClickedCancelEditTaskDescription task.id) ] [ Html.text "cancel" ]
                ]

        NotEditing taskDescription ->
            Html.div
                []
                [ Html.text (descriptionValue taskDescription)
                , Html.button [ HE.onClick (UserClickedEditTaskDescription task.id) ] [ Html.text "edit" ]
                ]


main =
    Browser.sandbox { init = init, update = update, view = view }
