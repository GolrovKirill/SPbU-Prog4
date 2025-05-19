module BracketSequence.Tests

open NUnit.Framework
open BracketSequence
open FsUnit

[<TestFixture>]
type ChechBracketTest () =
    [<Test>]
    member _.``Test empty string should be valid`` () =
        let input = ""
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test single pair of brackets ()`` () =
        let input = "()"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test single pair of brackets []`` () =
        let input = "[]"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test single pair of brackets {}`` () =
        let input = "{}"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test nested brackets ({[]})`` () =
        let input = "({[]})"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test mismatched brackets ([)]`` () =
        let input = "([)]"
        let result = checkBracket input
        result |> should equal false

    [<Test>]
    member _.``Test unbalanced brackets (`` () =
        let input = "("
        let result = checkBracket input
        result |> should equal false

    [<Test>]
    member _.``Test unbalanced brackets )`` () =
        let input = ")"
        let result = checkBracket input
        result |> should equal false

    [<Test>]
    member _.``Test mixed valid brackets ()[]{}`` () =
        let input = "()[]{}"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test invalid closing order }{`` () =
        let input = "}{"
        let result = checkBracket input
        result |> should equal false

    [<Test>]
    member _.``Test string with other characters (a[b]{c})`` () =
        let input = "a(b[c]{d}e)"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test invalid brackets (]`` () =
        let input = "(]"
        let result = checkBracket input
        result |> should equal false

    [<Test>]
    member _.``Test complex valid case ({()[]{}})"`` () =
        let input = "({()[]{}})"
        let result = checkBracket input
        result |> should equal true

    [<Test>]
    member _.``Test unbalanced with extra closing )`` () =
        let input = "())"
        let result = checkBracket input
        result |> should equal false

[<EntryPoint>]
let main _ = 0
