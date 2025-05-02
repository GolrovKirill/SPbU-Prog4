open PrimeNumber
open NUnit.Framework
open FsUnit


module PrimeTests =  
    
    [<Test>]
    let ``2 is a prime number`` () =
        selectPrimeNumber 2 |> should equal true

    [<Test>]
    let ``3 is a prime number`` () =
        selectPrimeNumber 3 |> should equal true

    [<Test>]
    let ``4 is not a prime number`` () =
        selectPrimeNumber 4 |> should equal false

    [<Test>]
    let ``5 is a prime number`` () =
        selectPrimeNumber 5 |> should equal true

    [<Test>]
    let ``Check first few primes`` () =
        let expectedPrimes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
        let actualPrimes = Seq.initInfinite (fun x -> x + 2) |> Seq.filter selectPrimeNumber |> Seq.take 10 |> Seq.toList

        actualPrimes |> should equal expectedPrimes
