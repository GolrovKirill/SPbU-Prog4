module MapTree

type BinaryTree = 
    | Node of value: int * left: BinaryTree * right: BinaryTree
    | Empty

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

// Create tree use CPS.
let createTreeCPS n =
    let rec create n cont =
        match n with
        | n when n < 1  -> cont Empty
        | n when n >= 1 -> 
            create (n - 1) (fun leftNode ->
                create (n - 1) (fun rightNode ->
                    cont (Node(n, leftNode, rightNode))))
    
    create n (fun x -> x)

// Create tree. 
let createTree n =
    let rec create n =
        match n with
        | n when n < 1  -> Empty
        | n when n >= 1 -> 
            Node(n, create (n - 1), create (n - 1))
    
    create n

// Use the function for all nodes in the tree.
// First method for traversal tree with use map.
let rec mapBinaryTree funcElement tree = 
    match tree with
    | Empty -> Empty
    | Node(v, left, right) ->  
        Node(funcElement v, (mapBinaryTree funcElement left), (mapBinaryTree funcElement right))

// Use the function for all nodes in the tree.
// Second method for traversal tree with use map and CPS.
let mapBinaryTreeCPS funcElement tree =
    let rec map funcElement tree cont = 
        match tree with
        | Empty -> cont Empty
        | Node(v, left, right) ->  
            map funcElement left (fun leftNode -> 
                map funcElement right (fun rightNode ->
                    cont (Node(funcElement v, leftNode, rightNode))))

    map funcElement tree (fun x -> x)

// Use the function for all nodes in the tree.
// Second method for traversal tree with use map and CPS with linearization.
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

// Create tree after use mapBinaryTreeIter.
let buildTree list =
    let len = List.length list
    
    let rec build list len cont =
        match list with
        | [] -> cont Empty
        | head :: tail ->
            let leftLen = len / 2
            let rightLen = len - leftLen - 1
            let left, right = List.splitAt (len / 2) tail
            build left leftLen (fun leftNode ->
                build right rightLen (fun rightNode ->
                    cont (Node(head, leftNode, rightNode))))

    build list len (fun x -> x)