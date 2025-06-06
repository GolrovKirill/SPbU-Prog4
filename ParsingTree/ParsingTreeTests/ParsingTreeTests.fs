﻿module ParsingTreeTests

open NUnit.Framework
open ParsingTree
open FsUnit

[<Test>]
let ``calculateParseTree should fail with "Entered empty tree." for empty tree`` () =
    let tree = EmptyNode
    calculateParseTree tree |> should equal None

[<Test>]
let ``calculateParseTree should fail with "Parse tree doesn't start with a number." for a single number`` () =
    let tree = Number(5.0)
    calculateParseTree tree |> should equal None

[<Test>]
let ``calculateParseTree should fail with "Division by 0." when dividing by zero`` () =
    let tree = Symbol("/", Number(20.0), Number(0.0))
    calculateParseTree tree |> should equal None

[<Test>]
let ``calculateParseTree should fail with "Entered incorrect operation." for invalid operation`` () =
    let tree = Symbol("invalid", Number(1.0), Number(2.0))
    calculateParseTree tree |> should equal None

[<Test>]
let ``calculateParseTree should calculate addition correctly`` () =
    let tree = Symbol("+", Number(3.0), Number(4.0))
    let result = calculateParseTree tree
    result |> should equal (Some 7.0)

[<Test>]
let ``calculateParseTree should calculate subtraction correctly`` () =
    let tree = Symbol("-", Number(10.0), Number(4.0))
    let result = calculateParseTree tree
    result |> should equal (Some 6.0)

[<Test>]
let ``calculateParseTree should calculate multiplication correctly`` () =
    let tree = Symbol("*", Number(2.0), Number(5.0))
    let result = calculateParseTree tree
    result |> should equal (Some 10.0)

[<Test>]
let ``calculateParseTree should calculate division correctly`` () =
    let tree = Symbol("/", Number(20.0), Number(5.0))
    let result = calculateParseTree tree
    result |> should equal (Some 4.0)

[<Test>]
let ``calculateParseTree should calculate all operation in deep tree`` () =
    let tree = Symbol("+", Symbol("*", Symbol("-", Number(1.0), Number(2.0)), Number(3.0)), Symbol("/", Number(4.0), Number(5.0)))
    let result = calculateParseTree tree
    result |> should equal (Some -2.2)

[<Test>]
let ``calculateParseTreeCPS should fail with "Entered empty tree." for empty tree`` () =
    let tree = EmptyNode
    calculateParseTreeCPS tree |> should equal None

[<Test>]
let ``calculateParseTreeCPS should fail with "Parse tree doesn't start with a number." for a single number`` () =
    let tree = Number(5.0)
    calculateParseTreeCPS tree |> should equal None

[<Test>]
let ``calculateParseTreeCPS should fail with "Division by 0." when dividing by zero`` () =
    let tree = Symbol("/", Number(20.0), Number(0.0))
    calculateParseTreeCPS tree |> should equal None

[<Test>]
let ``calculateParseTreeCPS should fail with "Entered incorrect operation." for invalid operation`` () =
    let tree = Symbol("invalid", Number(1.0), Number(2.0))
    calculateParseTreeCPS tree |> should equal None

[<Test>]
let ``calculateParseTreeCPS should calculate addition correctly`` () =
    let tree = Symbol("+", Number(3.0), Number(4.0))
    let result = calculateParseTreeCPS tree
    result |> should equal (Some 7.0)

[<Test>]
let ``calculateParseTreeCPS should calculate subtraction correctly`` () =
    let tree = Symbol("-", Number(10.0), Number(4.0))
    let result = calculateParseTreeCPS tree
    result |> should equal (Some 6.0)

[<Test>]
let ``calculateParseTreeCPS should calculate multiplication correctly`` () =
    let tree = Symbol("*", Number(2.0), Number(5.0))
    let result = calculateParseTreeCPS tree
    result |> should equal (Some 10.0)

[<Test>]
let ``calculateParseTreeCPS should calculate division correctly`` () =
    let tree = Symbol("/", Number(20.0), Number(5.0))
    let result = calculateParseTreeCPS tree
    result |> should equal (Some 4.0)

[<Test>]
let ``calculateParseTreeCPS should calculate all operation in deep tree`` () =
    let tree = Symbol("+", Symbol("*", Symbol("-", Number(1.0), Number(2.0)), Number(3.0)), Symbol("/", Number(4.0), Number(5.0)))
    let result = calculateParseTreeCPS tree
    result |> should equal (Some -2.2)

[<EntryPoint>]
let main argv =
    0