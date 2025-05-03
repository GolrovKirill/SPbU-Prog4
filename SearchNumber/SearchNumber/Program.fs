/// A function that searches for the position of a number in a passed list of integers.
let search list number =
    let rec searchNumber list number i =
        match list with
        | [] -> None
        | head::tail -> 
            match head with
            | head when head = number -> Some i
            | _ ->  searchNumber tail number (i + 1)

    searchNumber list number 0

let list = [1; 2; 3; 4; 4]
let result = search list 4
match result with
| Some result -> printfn $"{result}"
| None -> printfn "Absent"