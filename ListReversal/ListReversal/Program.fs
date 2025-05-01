/// The function that reverses a list takes a list of integers as a parameter.
let reversal list =
    match list with
    | [] -> []
    | _ ->
        let rec reversalList firstList secondList =
            match firstList with 
            | [] -> secondList
            | _ ->
                reversalList (List.tail firstList) ((List.head firstList) :: secondList)

        reversalList list []

let list = [1; 2; 3; 4]
let resultList = reversal list

for element in resultList do
    printf "%d " element


