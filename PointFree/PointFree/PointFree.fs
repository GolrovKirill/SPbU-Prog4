module PointFree

let func1 x l = List.map (fun y -> y * x) l

let func2 x = List.map (fun y -> y * x)

let func3 x = List.map ((*) x)

let func4 = List.map << (*)

let func5 : int -> List<int> -> List<int> = 
    (*) >> List.map
