module EvenNumber
    
    // Count even number in list use filter.
    let countEvenNumberWithFilter ls =
        ls |> List.filter (fun x -> x % 2 = 0) |> List.length

    // Count even number in list use map.
    let countEvenNumberWithMap ls =
        ls 
          |> List.map (fun x -> if x % 2 = 0 then Some x else None)
          |> List.choose id
          |> List.length

    // Count even number in list use fold.
    let countEvenNumberWithFold ls =
        ls |> List.fold(fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0