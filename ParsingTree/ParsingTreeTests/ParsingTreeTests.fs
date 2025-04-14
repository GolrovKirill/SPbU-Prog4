module ParsingTreeTests

open NUnit.Framework
open ParsingTree
open FsUnit

[<Test>]
let ``calculateParseTree should fail with "Entered empty tree." for empty tree`` () =
    let tree = EmptyNode
    (fun () -> (calculateParseTree tree) |> ignore)
    |> should throw typeof<System.Exception>

[<Test>]
let ``calculateParseTree should fail with "Parse tree doesn't start with a number." for a single number`` () =
    let tree = Number(5.0)
    (fun () -> (calculateParseTree tree) |> ignore)
    |> should throw typeof<System.Exception>

[<Test>]
let ``calculateParseTree should fail with "Division by 0." when dividing by zero`` () =
    let tree = Symbol("/", Number(20.0), Number(0.0))
    (fun () -> (calculateParseTree tree) |> ignore)
    |> should throw typeof<System.Exception>

[<Test>]
let ``calculateParseTree should fail with "Entered incorrect operation." for invalid operation`` () =
    let tree = Symbol("invalid", Number(1.0), Number(2.0))
    (fun () -> (calculateParseTree tree) |> ignore)
    |> should throw typeof<System.Exception>

[<Test>]
let ``calculateParseTree should calculate addition correctly`` () =
    let tree = Symbol("+", Number(3.0), Number(4.0))
    let result = calculateParseTree tree
    result |> should equal 7.0

[<Test>]
let ``calculateParseTree should calculate subtraction correctly`` () =
    let tree = Symbol("-", Number(10.0), Number(4.0))
    let result = calculateParseTree tree
    result |> should equal 6.0

[<Test>]
let ``calculateParseTree should calculate multiplication correctly`` () =
    let tree = Symbol("*", Number(2.0), Number(5.0))
    let result = calculateParseTree tree
    result |> should equal 10.0

[<Test>]
let ``calculateParseTree should calculate division correctly`` () =
    let tree = Symbol("/", Number(20.0), Number(5.0))
    let result = calculateParseTree tree
    result |> should equal 4.0

[<Test>]
let ``calculateParseTree should calculate all operation in deep tree`` () =
    let tree = Symbol("+", Symbol("*", Symbol("-", Number(1.0), Number(2.0)), Number(3.0)), Symbol("/", Number(4.0), Number(5.0)))
    let result = calculateParseTree tree
    result |> should equal -2.2