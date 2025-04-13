module MapTree

type BinaryTree = 
    | Node of value: int * left: BinaryTree option * right: BinaryTree option
    | Empty

let createBinaryTree value =
    Node(value, None, None)

let addNode tree valueLastNode sideLastNode value =
    let rec findNode tree valueLastNode sideLastNode =
        match tree with
        | Node(v, left, right) when v = valueLastNode ->
            match sideLastNode with
            | 0 -> 
                match left with
                | None -> Some tree
                | Some node -> findNode node valueLastNode 0
            | 1 ->
                match right with
                | None -> Some tree
                | Some node -> findNode node valueLastNode 1
            | _ -> 
                failwith "The wrong side is entered: 0 - on the left, 1 - on the right."
        | Node(v, left, right) when v <> valueLastNode ->
            match sideLastNode with
            | 0 -> 
                match left with
                | None -> failwith "This node does not exist or this node already has a descendant on this side."
                | Some node -> findNode node valueLastNode 0
            | 1 -> 
                match right with
                | None -> failwith "This node does not exist or this node already has a descendant on this side."
                | Some node -> findNode node valueLastNode 1
            | _ -> 
                failwith "The wrong side is entered: 0 - on the left, 1 - on the right."
        | _ -> 
            failwith "The wrong binary tree is entered."

    let newNode = Node(value, None, None)
    let nodeOption = findNode tree valueLastNode sideLastNode
    let node = 
        match nodeOption with
        | None -> failwith "The specified node does not exist."
        | Some n -> n

    let rec newTree tree node newNode sideLastNode =
        match sideLastNode with
        | 0 -> 
            match tree with
            | Node(v, left, right) when Node(v, left, right) = node ->
                Node(v, Some newNode, right)
            | Node(v, left, right) ->
                match newTree (Option.defaultValue Empty left) node newNode 0 with
                | Empty -> Node(v, left, right)
                | newLeft -> Node(v, Some newLeft, right)
            | Empty -> Empty
        | 1 -> 
            match tree with
            | Node(v, left, right) when Node(v, left, right) = node ->
                Node(v, left, Some newNode)
            | Node(v, left, right) ->
                match newTree (Option.defaultValue Empty right) node newNode 1 with
                | Empty -> Node(v, left, right)
                | newRight -> Node(v, left, Some newRight)
            | Empty -> Empty
        | _ -> failwith "The wrong side is entered: 0 - on the left, 1 - on the right."

    newTree tree node newNode sideLastNode

let rec mapBinaryTree funcElement tree = 
    match tree with
    | Empty -> Empty
    | Node(v, left, right) -> 
        let newValue = funcElement v
        let newLeft = 
            match left with
            | Some l -> Some (mapBinaryTree funcElement l)
            | None -> None 
        let newRight = 
            match right with
            | Some r -> Some (mapBinaryTree funcElement r)
            | None -> None
        
        Node(newValue, newLeft, newRight)
