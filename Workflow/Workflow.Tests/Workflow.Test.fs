module Workflow.Tests

open NUnit.Framework
open FsUnit
open Workflow

[<TestFixture>]
type TestsRoundWorkflow () =

    [<Test>]
    member _.``Basic calculation with 3 decimal places``() =
        let result = 
            rounding 3 {
                let! a = 2.0 / 12.0
                let! b = 3.5
                return a / b
            }

        result |> should equal 0.048 

    [<Test>]
    member _.``Rounding to zero decimal places``() =
        let result = 
            rounding 0 {
                let! a = 2.0 / 3.0
                return a
            }

        result |> should equal 1

    [<Test>]
    member _.``Multiple operations with intermediate rounding``() =
        let result = 
            rounding 1 {
                let! a = 10.0 / 3.0
                let! b = a * 2.0
                let! c = b + 1.5
                return c
            }

        result |> should equal 8.1

[<TestFixture>]
type TestsCalculateBuilder () =

    [<Test>]
    member _.``Correct numbers``() =
        let result = builder {
            let! x = "3"
            let! y = "2"
            let! z = "3"
            return (x + y) * z
        }

        result |> should equal (Some 15)

    [<Test>]
    member _.``Invalid number``() =
        let result = builder {
            let! x = "1"
            let! y = "Ъ"
            let z = x + y
            return z
        }

        result |> should equal None

[<EntryPoint>]
let main _ = 0
