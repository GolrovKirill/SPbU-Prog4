module MapTreeTests

open NUnit.Framework
open FsUnit
open MapTree

[<Test>]
let ``Test Creating a Binary Tree`` () =
    let tree = createBinaryTree 5
    tree |> should equal (Node(5, None, None))

[<Test>]
let ``Test Adding Nodes to the Tree`` () =
    let tree = createBinaryTree 5
    let updatedTree = addNode tree 5 0 3
    let finalTree = addNode updatedTree 5 1 7
    finalTree |> should equal (Node(5, Some(Node(3, None, None)), Some(Node(7, None, None))))

[<Test>]
let ``Test Adding Nodes to Non-existent Position Throws Error`` () =
    let tree = createBinaryTree 5
    (fun () -> addNode tree 6 0 3 |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Test Mapping Over the Tree`` () =
    let tree = createBinaryTree 5
    let updatedTree = addNode tree 5 0 3
    let finalTree = addNode updatedTree 5 1 7
    let mappedTree = mapBinaryTree (fun x -> x * 2) finalTree
    mappedTree |> should equal (Node(10, Some(Node(6, None, None)), Some(Node(14, None, None))))

[<Test>]
let ``Test Map on Empty Tree Returns Empty`` () =
    let tree = Empty
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal Empty
