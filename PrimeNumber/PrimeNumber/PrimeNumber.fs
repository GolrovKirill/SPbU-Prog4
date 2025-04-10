module PrimeNumber

let selectPrimeNumber x =
    if x < 2 then false
    else 
        let rec select i =
            if i * i > x then true
            else
                if x % i = 0 then false
                else select (i+1)
        select 2

let primeNumber = 
    Seq.initInfinite (fun x -> x + 2) |> Seq.filter selectPrimeNumber
