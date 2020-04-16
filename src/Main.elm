port module Main exposing (Id(..), Task, TaskDescription(..), TaskName(..), TaskViewState(..), deleteTask, main, nameValue, toggleTaskViewState)

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

-- import Date

import Browser
import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decoder
import Json.Encode as Encode
import Task
import Time



-- MODEL


type alias Model =
    { newTaskName : TaskName
    , newTaskDescription : TaskDescription
    , newTaskComment : CommentList
    , tasks : List Task
    , nextTaskId : Id
    , zone : Time.Zone
    , time : Time.Posix
    }


type alias Task =
    { name : Editable TaskName
    , description : Editable TaskDescription
    , id : Id
    , viewState : TaskViewState
    , status : TaskStatus
    , comments : CommentList
    }


taskEncoder : Task -> Encode.Value
taskEncoder task =
    Encode.object
        [ ( "name", Encode.string (nameValue (editableValue task.name)) )
        , ( "description", Encode.string (descriptionValue (editableValue task.description)) )
        , ( "id", Encode.int (intValue task.id) )
        , ( "status", Encode.string (statusValue task.status) )
        , ( "comments"
          , Encode.object
                [ ( "savedComments"
                  , Encode.list
                        (\c -> Encode.string (commentValue c))
                        task.comments.savedComments
                  )
                ]
          )
        ]


tasksEncoder : List Task -> Encode.Value
tasksEncoder tasks =
    Encode.list
        (\t -> taskEncoder t)
        tasks


type alias Time =
    Float


type Id
    = Id Int


type TaskName
    = TaskName String


type TaskDescription
    = TaskDescription String


type TaskComment
    = TaskComment String


type alias CommentList =
    { savedComments : List TaskComment
    , buffer : TaskComment
    }


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


init : String -> ( Model, Cmd Msg )
init localData =
    let
        decodedLocalData =
            Decoder.decodeString tasksDecoder localData

        tasks =
            case decodedLocalData of
                Ok decodedTaskList ->
                    decodedTaskList

                Err _ ->
                    []
    in
    ( { newTaskName = TaskName ""
      , newTaskDescription = TaskDescription ""
      , newTaskComment = { savedComments = [], buffer = TaskComment "" }
      , tasks = tasks
      , nextTaskId = Id 1
      , zone = Time.utc
      , time = Time.millisToPosix 0
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- , zone : Time.Zone
-- , time : Time.Posix
-- taskDecoder =
--     let
--         -- decodedValue : Decoder.Decoder a
--         decodedValue val =
--             case val of
--                 Ok a ->
--                     a
--                 Err _ ->
--                     ""
--     in
--     Decoder.map Task
--         (Decoder.succeed (NotEditing (TaskName (Decoder.at [ "name" ] Decoder.string |> Decoder.andThen (\value -> decodedValue value)))))
--         (Decoder.succeed (NotEditing (TaskDescription (Decoder.at [ "description" ] Decoder.string))))
--         (Decoder.succeed (Id (Decoder.at [ "id" ] Decoder.int)))
--         (Decoder.succeed Collapsed)
--         (Decoder.succeed Incomplete)
--         (Decoder.succeed { savedComments = [], buffer = TaskComment "" })
-- taskDecoder : Decoder.Decoder Task -> String


taskDecoder =
    -- let
    --     -- decodedValue : Decoder.Decoder a
    --     decodedValue val =
    --         case val of
    --             Ok a ->
    --                 a
    --             Err _ ->
    --                 ""
    -- in
    Decoder.map6 Task
        (Decoder.at [ "name" ] Decoder.string
            |> Decoder.andThen
                (\name ->
                    Decoder.succeed (NotEditing (TaskName name))
                )
        )
        -- (Decoder.succeed (NotEditing (TaskDescription (Decoder.at [ "description" ] Decoder.string))))
        (Decoder.at [ "description" ] Decoder.string
            |> Decoder.andThen
                (\description ->
                    Decoder.succeed (NotEditing (TaskDescription description))
                )
        )
        -- (Decoder.succeed (Id (Decoder.at [ "id" ] Decoder.int)))
        (Decoder.at [ "id" ] Decoder.int
            |> Decoder.andThen
                (\id ->
                    Decoder.succeed (Id id)
                )
        )
        (Decoder.succeed Collapsed)
        -- (Decoder.succeed Incomplete)
        (Decoder.at [ "status" ] Decoder.string
            |> Decoder.andThen
                (\status ->
                    case status of
                        "Complete" ->
                            Decoder.succeed Complete

                        _ ->
                            Decoder.succeed Incomplete
                )
        )
        -- (Decoder.succeed { savedComments = [], buffer = TaskComment "" })
        (Decoder.at [ "comments" ]
            commentListDecoder
         -- (Decoder.map2 CommentList
         --     (Decoder.at [ "savedComments" ]
         --         -- (Decoder.list commentDecoder)
         --         (Decoder.succeed [])
         --      -- |> Decoder.andThen
         --      --     (\comment ->
         --      --         Decoder.succeed (TaskComment comment)
         --      --     )
         --      -- (Decoder.at [ "name" ] Decoder.string
         --      --     |> Decoder.andThen
         --      --         (\name ->
         --      --             Decoder.succeed (NotEditing (TaskName name))
         --      --         )
         --      -- )
         --     )
         --     (Decoder.at
         --         [ "buffer" ]
         --         (Decoder.succeed (TaskComment ""))
         --     )
         --  -- (Decoder.succeed { buffer = TaskComment "" })
         -- )
        )


commentDecoder : Decoder.Decoder TaskComment
commentDecoder =
    Decoder.string
        |> Decoder.andThen
            (\comment ->
                Decoder.succeed (TaskComment comment)
            )


commentsDecoder : Decoder.Decoder (List TaskComment)
commentsDecoder =
    Decoder.list commentDecoder


commentListDecoder : Decoder.Decoder CommentList
commentListDecoder =
    Decoder.map2 CommentList
        -- (Decoder.succeed [ TaskComment "", TaskComment "" ])
        -- (Decoder.list commentDecoder)
        (Decoder.at [ "savedComments" ]
            commentsDecoder
        )
        (Decoder.succeed (TaskComment ""))



-- (Decoder.succeed { savedComments = [], buffer = TaskComment "" })
-- (Decoder.succeed (NotEditing (TaskDescription (Decoder.at [ "description" ] Decoder.string))))
-- , newTaskComment = { savedComments = [], buffer = TaskComment "" }


tasksDecoder : Decoder.Decoder (List Task)
tasksDecoder =
    Decoder.list taskDecoder



-- "[{"name":"take out trash","description":"milk, eggs, juice","id":4},{"name":"do dishes again","description":"make bed and vacuum","id":3},{"name":"buy groceries","description":"milk, eggs, juice","id":2},{"name":"clean room","description":"make bed and vacuum","id":1}]"
-- taskEncoder : Task -> Encode.Value
-- taskEncoder task =
--     Encode.object
--         [ ( "name", Encode.string (nameValue (editableValue task.name)) )
--         , ( "description", Encode.string (descriptionValue (editableValue task.description)) )
--         , ( "id", Encode.int (intValue task.id) )
--         ]
-- type alias Task =
--     { name : Editable TaskName
--     , description : Editable TaskDescription
--     , id : Id
--     , viewState : TaskViewState
--     , status : TaskStatus
--     , comments : CommentList
--     }
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
    | UserEditedTaskComment Id TaskComment
    | UserClickedAddComment Id
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


port persistTasks : Encode.Value -> Cmd msg



-- | UserClickedEditTaskComment Id
-- | UserClickedSaveEditTaskComment Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedAddTask ->
            -- {
            --     newTaskDescription = ""
            --     , nextTaskId = model.nextTaskId
            --     , tasks = model.tasks
            -- }
            ( { model
                | newTaskDescription = TaskDescription ""
                , newTaskName = TaskName ""
              }
                |> addTask model.newTaskName model.newTaskDescription { savedComments = [], buffer = TaskComment "" }
            , Cmd.none
            )

        UserEditedNewTaskName newName ->
            ( { model | newTaskName = newName }
            , Cmd.none
            )

        UserEditedNewTaskDescription newDescription ->
            ( { model | newTaskDescription = newDescription }
            , Cmd.none
            )

        UserClickedDeleteTask taskId ->
            ( { model
                | tasks = deleteTask taskId model.tasks
              }
            , Cmd.none
            )

        UserClickedToggleTaskViewState taskId ->
            ( { model
                | tasks = toggleTaskViewState taskId model.tasks
              }
            , Cmd.none
            )

        UserClickedEditTaskName taskId ->
            ( { model
                | tasks = editTaskForId taskId startEditingName model.tasks
              }
            , Cmd.none
            )

        UserClickedSaveEditTaskName taskId ->
            let
                updatedModel =
                    { model
                        | tasks = editTaskForId taskId stopEditingName model.tasks
                    }
            in
            ( updatedModel
            , persistTasks (tasksEncoder updatedModel.tasks)
            )

        UserClickedCancelEditTaskName taskId ->
            ( { model
                | tasks = editTaskForId taskId cancelEditingName model.tasks
              }
            , Cmd.none
            )

        UserEditedTaskName taskId editedTaskName ->
            ( { model
                | tasks = editTaskForId taskId (setNameBuffer editedTaskName) model.tasks
              }
            , Cmd.none
            )

        UserClickedEditTaskDescription taskId ->
            ( { model
                | tasks = editTaskForId taskId startEditingDescription model.tasks
              }
            , Cmd.none
            )

        UserClickedSaveEditTaskDescription taskId ->
            ( { model
                | tasks = editTaskForId taskId stopEditingDescription model.tasks
              }
            , Cmd.none
            )

        UserClickedCancelEditTaskDescription taskId ->
            ( { model
                | tasks = editTaskForId taskId cancelEditingDescription model.tasks
              }
            , Cmd.none
            )

        UserEditedTaskDescription taskId editedTaskDescription ->
            ( { model
                | tasks = editTaskForId taskId (setDescriptionBuffer editedTaskDescription) model.tasks
              }
            , Cmd.none
            )

        UserClickedUpdateStatus taskId ->
            ( { model
                | tasks = editTaskForId taskId toggleStatus model.tasks
              }
            , Cmd.none
            )

        UserEditedTaskComment taskId editedTaskComment ->
            ( { model
                | tasks = editTaskForId taskId (setCommentBuffer editedTaskComment) model.tasks
              }
            , Cmd.none
            )

        UserClickedAddComment taskId ->
            ( { model
                | tasks = editTaskForId taskId addComment model.tasks
              }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- { model
--     | tasks = editTaskForId taskId (setCommentBuffer editedTaskComment) model.tasks
-- }
-- UserClickedEditTaskComment taskId ->
--     { model
--         | tasks = editTaskForId taskId startEditingComment model.tasks
--     }
-- UserClickedSaveEditTaskComment taskId ->
--     { model
--         | tasks = editTaskForId taskId stopEditingComment model.tasks
--     }


addTask : TaskName -> TaskDescription -> CommentList -> Model -> Model
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


editableValue : Editable a -> a
editableValue value =
    case value of
        NotEditing a ->
            a

        Editing { originalValue } ->
            originalValue


intValue : Id -> Int
intValue (Id int) =
    int


nameValue : TaskName -> String
nameValue (TaskName name) =
    name


descriptionValue : TaskDescription -> String
descriptionValue (TaskDescription description) =
    description


commentValue : TaskComment -> String
commentValue (TaskComment comment) =
    comment


statusValue : TaskStatus -> String
statusValue status =
    case status of
        Complete ->
            "Complete"

        Incomplete ->
            "Incomplete"


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


setCommentBuffer : TaskComment -> Task -> Task
setCommentBuffer newComment task =
    { task
        | comments = { savedComments = task.comments.savedComments, buffer = newComment }
    }



-- stopEditingDescription : Task -> Task
-- stopEditingDescription task =
--     { task
--         | description = stopEditing task.description
--     }


addComment : Task -> Task
addComment task =
    { task
        | comments = { savedComments = task.comments.savedComments ++ [ task.comments.buffer ], buffer = TaskComment "" }
    }



-- setCommentBuffer : TaskComment -> Task -> Task
-- setCommentBuffer newComment task =
--     { task
--         | comments =
--             case comments of
--                 Editing { originalValue } ->
--                     Editing { originalValue = originalValue, buffer = newComment }
--                 NotEditing _ ->
--                     task.comments
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
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
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
        -- , Html.h2 [] [ Html.text Time ]
        , Html.h1 [] [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second) ]
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
            Html.button [ HE.onClick (UserClickedDeleteTask task.id), HA.class "taskDeleteButton level-right" ] [ Html.text "delete task" ]

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
        [ Html.div
            []
            (task.comments.savedComments
                |> List.map
                    (\taskComment ->
                        Html.div
                            [ HA.class "task-description" ]
                            [ Html.text (commentValue taskComment)
                            ]
                    )
            )
        , Html.input
            [ HE.onInput (\comment -> TaskComment comment |> UserEditedTaskComment task.id)
            , HA.value (commentValue task.comments.buffer)
            , HA.class "newCommentInput"
            ]
            []
        , Html.button
            [ HE.onClick (UserClickedAddComment task.id) ]
            [ Html.text "add comment" ]
        ]



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
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Time.every 1000 Tick


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
