module PrimeNumber

/// Function that checks the primality of a number.
let selectPrimeNumber x =
    match x with
    | x when x < 2 -> false
    | _ -> 
        let rec select i =
            match i with
            | i when i * i > x -> true
            | _ -> 
                match x with
                | x when x % i = 0 -> false
                | _ -> select (i + 1)
        select 2

/// A function that generates a sequence of prime numbers.
let sequencePrime = Seq.initInfinite (fun x -> x + 2) |> Seq.filter selectPrimeNumber