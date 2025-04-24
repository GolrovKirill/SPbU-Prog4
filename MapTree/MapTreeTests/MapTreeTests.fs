module MapTreeTests

open NUnit.Framework
open FsUnit
open MapTree

[<Test>]
let ``Test mapBinaryTree with tree`` () =
    let tree = (Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty)))
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal (Node(10, Node(6, Empty, Empty), Node(14, Empty, Empty)))

[<Test>]
let ``Test mapBinaryTree with empty tree`` () =
    let tree = Empty
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal Empty

[<Test>]
let ``Test iterMapBinaryTree with tree`` () =
    let tree = (Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty)))
    let result = ref []
    iterMap (fun x -> result := (x * 2)::!result) tree
    !result |> should equal [14; 6; 10]

[<Test>]
let ``Test iterMap with empty tree`` () =
    let tree = Empty
    let result = ref []
    iterMap (fun x -> result := (x * 2)::!result) tree
    (!result |> List.isEmpty) |> should be True
