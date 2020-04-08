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

7.  TRY (stretch goal) One thing to look at adding is the completion status,
    as well as a button to view complete/incomplete/all tasks.
    That should use most all that we’ve done so far as practice.

8.  integrate Bulma styling

9.  toggle arrow for expanded an unexpanded items

10. allow comments for tasks

11. add filters

12. save tasks into local storage

13. integrate fa icons

-}

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type alias Model =
    { newTaskName : TaskName
    , newTaskDescription : TaskDescription
    , newTaskComment : TaskComment
    , tasks : List Task
    , nextTaskId : Id
    }


type alias Task =
    { name : Editable TaskName
    , description : Editable TaskDescription
    , id : Id
    , viewState : TaskViewState
    , status : TaskStatus
    , comments : List TaskComment
    }


type Id
    = Id Int


type TaskName
    = TaskName String


type TaskDescription
    = TaskDescription String


type TaskComment
    = TaskComment String


type TaskStatus
    = Complete
    | Incomplete


type Editable a
    = NotEditing a
    | Editing { originalValue : a, buffer : a }


type TaskViewState
    = Expanded
    | Collapsed



-- type EditableName
--     = NotEditingName TaskName
--     | EditingName { originalValue : TaskName, buffer : TaskName }
-- type EditableDescription
--     = NotEditingDescription TaskDescription
--     | EditingDescription { originalDescription : TaskDescription, descriptionBuffer : TaskDescription }
-- type TaskEditableField
--     = TaskEditableName
--     | TaskEditableDescription


init : Model
init =
    { newTaskName = TaskName ""
    , newTaskDescription = TaskDescription ""
    , newTaskComment = TaskComment ""
    , tasks =
        []
    , nextTaskId = Id 1
    }
        |> addTask (TaskName "clean room") (TaskDescription "make bed and vacuum") [ TaskComment "3", TaskComment "4" ]
        |> addTask (TaskName "buy groceries") (TaskDescription "milk, eggs, juice") [ TaskComment "1", TaskComment "2" ]
        |> addTask (TaskName "do dishes") (TaskDescription "make bed and vacuum") []
        |> addTask (TaskName "take out trash") (TaskDescription "milk, eggs, juice") [ TaskComment "5", TaskComment "6" ]



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
    | UserClickedUpdateStatus Id



-- | UserEditedTaskComment Id TaskComment
-- | UserClickedEditTaskComment Id
-- | UserClickedSaveEditTaskComment Id


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
                |> addTask model.newTaskName model.newTaskDescription []

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
                | tasks = editTaskForId taskId (setNameBuffer editedTaskName) model.tasks
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
                | tasks = editTaskForId taskId (setDescriptionBuffer editedTaskDescription) model.tasks
            }

        UserClickedUpdateStatus taskId ->
            { model
                | tasks = editTaskForId taskId toggleStatus model.tasks
            }



-- UserEditedTaskComment taskId editedTaskComment ->
--     { model
--         | tasks = editTaskForId taskId (setCommentBuffer editedTaskComment) model.tasks
--     }
-- UserClickedEditTaskComment taskId ->
--     { model
--         | tasks = editTaskForId taskId startEditingComment model.tasks
--     }
-- UserClickedSaveEditTaskComment taskId ->
--     { model
--         | tasks = editTaskForId taskId stopEditingComment model.tasks
--     }


addTask : TaskName -> TaskDescription -> List TaskComment -> Model -> Model
addTask name description comments model =
    { model
        | tasks = { name = NotEditing name, id = model.nextTaskId, description = NotEditing description, viewState = Collapsed, status = Incomplete, comments = comments } :: model.tasks
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


commentValue : TaskComment -> String
commentValue (TaskComment comment) =
    comment



-- { model
--     | tasks = updateTaskDescriptionBuffer taskId model.tasks
-- }


deleteTask : Id -> List Task -> List Task
deleteTask id tasks =
    List.filter (\task -> task.id /= id) tasks


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



-- startEditingComment : Task -> Task
-- startEditingComment task =
--     { task
--         | comments = startEditing task.comments
--     }
-- addTaskComment : Task -> Task
-- addTaskComment task =
--     { task
--         | comments = TaskComment "yo"
--     }
-- deleteTask : Id -> List Task -> List Task
-- deleteTask id =
--     List.filter (\task -> task.id /= id)


updateBuffer : Editable a -> a -> Editable a
updateBuffer value temp =
    case value of
        Editing { originalValue } ->
            Editing { originalValue = originalValue, buffer = temp }

        NotEditing _ ->
            value


setNameBuffer : TaskName -> Task -> Task
setNameBuffer newBuffer task =
    { task
        | name = updateBuffer task.name newBuffer
    }


setDescriptionBuffer : TaskDescription -> Task -> Task
setDescriptionBuffer newDescription task =
    { task
        | description = updateBuffer task.description newDescription
    }



-- setCommentBuffer : TaskComment -> Task -> Task
-- setCommentBuffer newBuffer task =
--     { task
--         | comments = updateBuffer task.comments newBuffer
--     }
-- setCommentBuffer : TaskComment -> Task -> Task
-- setCommentBuffer newBuffer task =
--     { task
--         | comment = updateBuffer task.comment newBuffer
--     }


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



-- stopEditingComment : Task -> Task
-- stopEditingComment task =
--     { task
--         | comments = stopEditing task.comments
--     }


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


toggleStatus task =
    case task.status of
        Complete ->
            { task
                | status = Incomplete
            }

        Incomplete ->
            { task
                | status = Complete
            }



-- toggleTaskStatus : Id -> List Task -> List Task
-- toggleTaskStatus id tasks =
--     let
--     in
--     editTaskForId id toggleStatus tasks


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



-- VIEW


css path =
    Html.node "link" [ HA.rel "stylesheet", HA.href "/style.css" ] []


view : Model -> Html.Html Msg
view model =
    -- Html.div []
    --     (newTaskView model
    --         :: List.map taskView model.tasks
    --     )
    Html.div [ HA.class "content-container container box box-shadow" ]
        [ Html.h1 [ HA.class "title is-6 level-item" ] [ Html.text "Elm To-do List Tester" ]
        , newTaskView model
        , incompleteTasksView model
        , completeTasksView model

        -- , Html.div []git s
        --     [ Html.button [ ] [ Html.text "add" ]
        --     , Html.button [ ] [ Html.text "add" ]
        --     , Html.button [ ] [ Html.text "add" ]
        --     , Html.button [ ] [ Html.text "add" ]
        --     , Html.button [ ] [ Html.text "add" ]
        --     , Html.button [ ] [ Html.text "add" ]
        --     ]
        ]


newTaskView : Model -> Html.Html Msg
newTaskView model =
    Html.div []
        [ Html.input
            [ HA.placeholder "Name"
            , HA.value <| nameValue model.newTaskName
            , HE.onInput <| TaskName >> UserEditedNewTaskName
            , HA.class "newTaskNameInput level tile is-12"
            ]
            []
        , Html.textarea
            [ HA.placeholder "Description"
            , HA.value <| descriptionValue model.newTaskDescription
            , HE.onInput (\taskName -> TaskDescription taskName |> UserEditedNewTaskDescription)
            , HA.class "newTaskDescriptionInput level tile is-12"
            , HA.rows 3
            ]
            []
        , Html.button
            [ HE.onClick UserClickedAddTask
            , HA.class "newTaskAddButton is-pulled-right"
            ]
            [ Html.text "add" ]
        ]


incompleteTasksView model =
    Html.div []
        [ Html.h2 [ HA.class "title is-5" ] [ Html.text "Pending Tasks" ]
        , Html.div [ HA.class "incomplete-tasks" ]
            -- ( List.map taskView (List.filter (\task -> task.status == Complete )model.tasks) )
            (List.map taskView <| List.filter (\task -> task.status /= Complete) model.tasks)
        ]



-- completeTasksView : Model -> Html.Html Msg


completeTasksView model =
    Html.div []
        [ Html.h2 [ HA.class "title is-5" ] [ Html.text "Completed Tasks" ]
        , Html.div [ HA.class "complete-tasks" ]
            -- ( List.map taskView (List.filter (\task -> task.status == Complete )model.tasks) )
            (List.map taskView <| List.filter (\task -> task.status == Complete) model.tasks)
        ]


taskView : Task -> Html.Html Msg
taskView task =
    let
        deleteButtonView =
            Html.button [ HE.onClick (UserClickedDeleteTask task.id), HA.class "taskDeleteButton level-right" ] [ Html.text "delete" ]

        updateStatusButtonView =
            Html.button [ HE.onClick (UserClickedUpdateStatus task.id), HA.class "updateStatusButton level-right" ] [ Html.text "toggle complete" ]
    in
    case task.viewState of
        Collapsed ->
            Html.div [ HA.class "task-container" ]
                [ Html.div [ HA.class "level" ]
                    [ taskNameView task
                    , deleteButtonView
                    , updateStatusButtonView
                    ]
                ]

        Expanded ->
            Html.div [ HA.class "task-container" ]
                [ taskNameView task
                , taskDescriptionView task
                , taskCommentsView task
                , deleteButtonView
                , updateStatusButtonView
                ]


taskNameView : Task -> Html.Html Msg
taskNameView task =
    case task.name of
        Editing { buffer } ->
            Html.div
                [ HA.class "task-name level-right" ]
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
                [ HA.class "task-name level-left" ]
                [ Html.span [ HE.onClick <| UserClickedToggleTaskViewState task.id ] [ Html.text (nameValue taskName) ]
                , Html.button [ HE.onClick (UserClickedEditTaskName task.id) ] [ Html.text "edit" ]
                ]


taskDescriptionView : Task -> Html.Html Msg
taskDescriptionView task =
    case task.description of
        Editing { buffer } ->
            Html.div
                [ HA.class "task-description" ]
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
                [ HA.class "task-description" ]
                [ Html.text (descriptionValue taskDescription)
                , Html.button [ HE.onClick (UserClickedEditTaskDescription task.id) ] [ Html.text "edit" ]
                ]


taskCommentsView : Task -> Html.Html Msg
taskCommentsView task =
    -- List.map (\item => item) task.comments
    Html.div
        []
        (task.comments
            |> List.map
                (\taskComment ->
                    Html.div
                        []
                        [ Html.text (commentValue taskComment)
                        ]
                )
        )



-- Editing { buffer } ->
--     Html.div
--         []
--         [ Html.input
--             [ HE.onInput (\comment -> TaskComment comment |> UserEditedTaskComment task.id)
--             , HA.value (commentValue buffer)
--             ]
--             []
--         , Html.button [ HE.onClick (UserClickedSaveEditTaskComment task.id) ] [ Html.text "save" ]
--         , Html.button [ HE.onClick (UserClickedCancelEditTaskDescription task.id) ] [ Html.text "cancel" ]
--         ]
-- NotEditing taskComment ->
--     Html.div
--         []
--         [ Html.text (commentValue taskComment)
--         , Html.button [ HE.onClick (UserClickedEditTaskComment task.id) ] [ Html.text "edit" ]
--         ]


main =
    Browser.sandbox { init = init, update = update, view = view }
