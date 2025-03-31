let rec factorial x acc =
    if x < 0 || x > 31 then // For numbers greater than 31, you need to use libraries for counting large numbers 
        failwith "Incorrect number entered"
    elif x <= 1 then 
        acc
    else 
        factorial (x - 1) (acc * x)

[<EntryPoint>]
let main argv =
    let x = 31
    try
        let result = factorial x 1
        printfn "Result = %d" result
    with
    | ex -> printfn "Ошибка: %s" ex.Message
    0