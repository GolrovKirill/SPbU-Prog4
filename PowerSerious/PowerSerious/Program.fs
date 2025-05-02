/// A function that reverses an input list of integers.
let revers list = 
    match list with
    | [] -> []
    | _ ->
        let rec reversList firstList secondList =
            match firstList with
            | [] -> secondList
            | _ ->
                reversList (List.tail firstList) ((List.head firstList) :: secondList)

        reversList list []

/// A function that constructs a series of powers of two from the first given parameter to the second.
let listPowerNumber n m =
    let rec listPower i m num list =
        match i with
        | i when i = m ->
            (num * 2) :: list
        | _ ->
            listPower (i + 1) m (num * 2) (num :: list)

    match (n, m) with
    |(n, m) when n <= m ->
        let num = 1 <<< n
        let listResult = listPower 0 m num []
        revers listResult
    | _ -> 
        []

let printList = listPowerNumber 1 3
match printList with
| [] -> printf "Empty list"
| _ -> 
    for element in printList do
        printf $"{element} "