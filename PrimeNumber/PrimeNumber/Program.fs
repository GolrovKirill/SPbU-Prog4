let selectPrimeNumber x =
    let rec select i =
        if i > int (sqrt (float x)) then true
        elif (x % i = 0) || x < 1 then false
        else select (i + 1)

    select 2

let primeNumber = 
    Seq.initInfinite (fun x -> x + 2)
    |> Seq.filter selectPrimeNumber

primeNumber
|> Seq.take 10
|> Seq.iter (printfn "%d")