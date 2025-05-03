module MapTree

type BinaryTree = 
    | Node of value: int * left: BinaryTree * right: BinaryTree
    | Empty

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

/// A function that creates a tree with nodes in descending order from the root, which is an input integer use CPS. 
let createTreeCPS n =
    let rec create n cont =
        match n with
        | n when n >= 1 -> 
            create (n - 1) (fun leftNode ->
                create (n - 1) (fun rightNode ->
                    cont (Node(n, leftNode, rightNode))))
        | _ -> cont Empty

    create n id

/// A function that creates a tree with nodes in descending order from the root, which is an input integer. 
let createTree n =
    let rec create n =
        match n with
        | n when n >= 1 ->
            Node(n, create (n - 1), create (n - 1))
        | _ -> Empty
    
    create n

/// A function that traversal a tree сonsisting of integers with use map which return new tree.
let rec mapBinaryTree funcElement tree = 
    match tree with
    | Empty -> Empty
    | Node(v, left, right) ->
        Node(funcElement v, (mapBinaryTree funcElement left), (mapBinaryTree funcElement right))

/// A function that traversal a tree сonsisting of integers with use map and CPS which return new tree.
let mapBinaryTreeCPS funcElement tree =
    let rec map funcElement tree cont = 
        match tree with
        | Empty -> cont Empty
        | Node(v, left, right) ->  
            map funcElement left (fun leftNode -> 
                map funcElement right (fun rightNode ->
                    cont (Node(funcElement v, leftNode, rightNode))))

    map funcElement tree id

/// Function that traverses a tree of integers using map and CPS with linearization, which returns a list of integers.
let mapBinaryTreeIter f tree =
    let rec linearize tree cont =
        match tree with
        | Empty -> cont()
        | Node(x, l, r) ->
            Step(x, (fun () -> linearize l (fun () ->
                linearize r cont)))

    let rec processSteps step acc =
        match step with
        | Finished -> List.rev acc
        | Step(x, getNext) -> 
            processSteps (getNext()) ((f x) :: acc)
    
    processSteps (linearize tree (fun () -> Finished)) []