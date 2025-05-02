module EvenNumber
    
    /// The function to count even numbers in a list uses a filter.
    let countEvenNumberWithFilter ls =
        ls |> List.filter (fun x -> x % 2 = 0) |> List.length

    /// The function to count even numbers in a list uses a map.
    let countEvenNumberWithMap ls =
        ls 
            |> List.map (fun x -> if x % 2 = 0 then Some x else None)
            |> List.choose id
            |> List.length

    /// The function to count even numbers in a list uses a fold.
    let countEvenNumberWithFold ls =
        ls |> List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0