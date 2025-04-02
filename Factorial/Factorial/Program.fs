let rec factorial x acc =
    if x < 0.0 || x > 31.0 || (x % 1.0 <> 0.0) then // For numbers greater than 31, you need to use libraries for counting large numbers 
        failwith "Incorrect number entered"
    elif x <= 1.0 then 
        acc
    else 
        factorial (x - 1.0) (acc * x)

[<EntryPoint>]
let main argv =
    let x = -2.765
    let num = float x
    try
        let result = factorial num 1.0
        printfn $"Result = {result}"
    with
    | ex -> printfn "Ошибка: %s" ex.Message
    0