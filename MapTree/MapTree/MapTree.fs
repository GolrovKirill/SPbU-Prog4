module MapTree

type BinaryTree = 
    | Node of value: int * left: BinaryTree * right: BinaryTree
    | Empty

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

// Use the function for all nodes in the tree.
// First method for traversal tree with use map.
let rec mapBinaryTree funcElement tree = 
    match tree with
    | Empty -> Empty
    | Node(v, left, right) ->  
        Node(funcElement v, (mapBinaryTree funcElement left), (mapBinaryTree funcElement right))

// Traversing the tree to build a sequence of steps.
let rec linearize tree cont =
   match tree with
   | Empty -> cont()
   | Node(v, left, right) -> 
       Step(v, (fun () -> linearize left (fun () -> linearize right cont)))

// Use the function for all nodes in the tree.
// Second method for traversal tree with use map.
let iterMap funcElement tree =
   let steps = linearize tree (fun() -> Finished)

   let rec iter funcElement step =
       match step with
       | Finished -> ()
       | Step(v, next) ->
           funcElement v
           iter funcElement (next())
    
   iter funcElement steps

// need function for create tree after iterMap.