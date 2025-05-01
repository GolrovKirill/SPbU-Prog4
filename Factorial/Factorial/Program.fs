/// The function returns the factorial of an integer x
let factorial x =
    let rec calculate x acc = 
        match x with
        | Some x ->
            match x with
            | x when (x < 0.0 || x > 31.0 || (x <> floor x)) -> None
            | x when (x <= 0.0) -> (Some acc)
            | _ -> calculate (Some(x - 1.0)) (acc * x)
        | None -> None
    
    calculate x 1.0
        

[<EntryPoint>]
let main argv =
    let x = 5
    let num = float x
    try
        let result = factorial (Some num)
        match result with
        | Some result -> printfn $"Result = {result}"
        | None -> printfn "Result = None"       
    with
    | ex -> printfn "Ошибка: %s" ex.Message
    0
