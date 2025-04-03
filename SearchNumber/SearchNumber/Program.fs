let search list number =
    let rec searchNumber list number i =
        if List.isEmpty list then
            None
        elif number = List.head list then
            Some i
        else searchNumber (List.tail list) number (i + 1)
       
    searchNumber list number 0

let list = [1; 2; 3; 4; 4]
let result = search list 4
printfn $"{result}"