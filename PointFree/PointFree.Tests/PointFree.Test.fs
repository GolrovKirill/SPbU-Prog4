module PointFree.Tests

open NUnit.Framework
open PointFree
open FsUnit
open FsCheck

[<Test>]
let ``Test func1`` () =
    let result = func1 2 [1; 2; 4]
    result |> should equal [2; 4; 8]

[<Test>]
let ``Test func2`` () =
    let result = func2 2 [1; 2; 4]
    result |> should equal [2; 4; 8]

[<Test>]
let ``Test func2'`` () =
    let result = func2' 2 [1; 2; 4]
    result |> should equal [2; 4; 8]

[<Test>]
let ``Test with use FsCheck`` () =
    let test (num: int) (ls: list<int>) = 
        let resultFunc1 = func1 num ls
        let resultFunc2 = func2 num ls
        let resultFunc2' = func2' num ls

        resultFunc1 = resultFunc2 && resultFunc2 = resultFunc2'
    Check.QuickThrowOnFailure test

[<EntryPoint>]
let main _ = 0

