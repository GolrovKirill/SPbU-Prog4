module MapTreeTests

open NUnit.Framework
open FsUnit
open MapTree

[<Test>]
let ``Test correct create simple tree with createTree`` () =
    let tree = createTree 3 
    tree |> should equal (Node(3, Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty))))

[<Test>]
let ``Test correct create simple tree with createTreeCPS`` () =
    let tree = createTreeCPS 3 
    tree |> should equal (Node(3, Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty))))

[<Test>]
let ``Test mapBinaryTree with tree`` () =
    let tree = createTreeCPS 3 
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal (Node(6, Node(4, Node(2, Empty, Empty), Node(2, Empty, Empty)), Node(4, Node(2, Empty, Empty), Node(2, Empty, Empty))))

[<Test>]
let ``Test mapBinaryTree with empty tree`` () =
    let tree = Empty
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal Empty

[<Test>]
let ``Test mapBinaryTreeCPS with tree`` () =
    let tree = createTreeCPS 2 
    let mappedTree = mapBinaryTreeCPS (fun x -> x * 2) tree
    mappedTree |> should equal (Node(4, Node(2, Empty, Empty), Node(2, Empty, Empty)))

[<Test>]
let ``Test mapBinaryTreeCPS with empty tree`` () =
    let tree = Empty
    let mappedTree = mapBinaryTreeCPS (fun x -> x * 2) tree
    mappedTree |> should equal Empty

[<Test>]
let ``Test mapBinaryTreeIter with tree`` () =
    let tree = createTreeCPS 3
    let mappedList = mapBinaryTreeIter (fun x -> x * 2) tree
    mappedList |> should equal [6; 4; 2; 2; 4; 2; 2]

[<Test>]
let ``Test mapBinaryTreeIter with empty tree`` () =
    let tree = Empty
    let mappedTree = mapBinaryTree (fun x -> x * 2) tree
    mappedTree |> should equal Empty


