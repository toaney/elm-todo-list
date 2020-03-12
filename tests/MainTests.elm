module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Id(..), Task, TaskViewState(..))
import Test exposing (..)


suite : Test
suite =
    describe "ToDo App"
        [ deletingTaskTests
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
                    |> Expect.equalLists [ { description = "clean room description", id = Id 1, name = "clean room name", viewState = Collapsed } ]
        ]



-- type alias Task =
--     { name : String
--     , description : String
--     , id : Id
--     , viewState : TaskViewState
--     }


taskList : List Task
taskList =
    [ { description = "clean room description", id = Id 1, name = "clean room name", viewState = Collapsed }
    , { description = "buy groceries description", id = Id 2, name = "clean room name", viewState = Collapsed }
    ]
