module MainTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Task)
import Test exposing (..)


suite : Test
suite =
    describe "Deleting a task"
        [ test "Empty list returns an empty list" <|
            \_ ->
                Main.deleteTask 1 []
                    |> Expect.equalLists []
        , test "Returns unmodified list when invalid task id is passed" <|
            \_ ->
                Main.deleteTask 9 taskList
                    |> Expect.equalLists taskList
        , test "Removes task for given id" <|
            \_ ->
                Main.deleteTask 2 taskList
                    |> Expect.equalLists [ { description = "clean room", id = 1 } ]
        ]


taskList : List Task
taskList =
    [ { description = "clean room", id = 1 }
    , { description = "buy groceries", id = 2 }
    ]
