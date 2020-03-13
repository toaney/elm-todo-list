module MainTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (EditableDescription(..), EditableName(..), Id(..), Task, TaskDescription(..), TaskName(..), TaskViewState(..))
import Test exposing (..)


suite : Test
suite =
    describe "ToDo App"
        [ deletingTaskTests
        , toggleTaskViewStateTests
        ]


deletingTaskTests : Test
deletingTaskTests =
    describe "Deleting a task"
        [ test "Empty list returns an empty list" <|
            \_ ->
                Main.deleteTask (Id 1) []
                    |> Expect.equalLists []
        , test "Returns unmodified list when invalid task id is passed" <|
            \_ ->
                Main.deleteTask (Id 9) taskList
                    |> Expect.equalLists taskList
        , test "Removes task for given id" <|
            \_ ->
                Main.deleteTask (Id 2) taskList
                    |> Expect.equalLists [ { name = NotEditingName (TaskName "task1"), id = Id 1, description = NotEditingDescription (TaskDescription "task1 description"), viewState = Collapsed } ]
        ]


toggleTaskViewStateTests : Test
toggleTaskViewStateTests =
    describe "Toggling viewState on a task"
        [ test "Toggle task viewState from Collapsed to Expanded" <|
            \_ ->
                Main.toggleTaskViewState (Id 1) taskList
                    |> Expect.equalLists
                        [ { name = NotEditingName (TaskName "task1"), id = Id 1, description = NotEditingDescription (TaskDescription "task1 description"), viewState = Expanded }
                        , { name = NotEditingName (TaskName "task2"), id = Id 2, description = NotEditingDescription (TaskDescription "task2 description"), viewState = Collapsed }
                        ]
        , test "Toggle task viewState twice ending in viewState Collapsed" <|
            \_ ->
                Main.toggleTaskViewState (Id 1) taskList
                    |> Main.toggleTaskViewState (Id 1)
                    |> Expect.equalLists taskList
        ]



-- type alias Task =
--     { name : EditableName
--     , description : EditableDescription
--     , id : Id
--     , viewState : TaskViewState
--     }


taskList : List Task
taskList =
    [ { name = NotEditingName (TaskName "task1"), id = Id 1, description = NotEditingDescription (TaskDescription "task1 description"), viewState = Collapsed }
    , { name = NotEditingName (TaskName "task2"), id = Id 2, description = NotEditingDescription (TaskDescription "task2 description"), viewState = Collapsed }
    ]



-- |> addTask (TaskName "clean room") (TaskDescription "make bed and vacuum")
-- |> addTask (TaskName "buy groceries") (TaskDescription "milk, eggs, juice")
