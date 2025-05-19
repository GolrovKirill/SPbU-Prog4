module PointFree

let func1 x l = List.map (fun y -> y * x) l

let func2 : int -> List<int> -> List<int> = 
    (*) >> List.map

let func2' = List.map << (*)