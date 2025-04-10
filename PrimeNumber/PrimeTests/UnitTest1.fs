module PrimeTests

open NUnit.Framework
open FsUnit
open PrimeNumber

[<TestFixture>]
type PrimeTests() =

    [<Test>]
    member _.``2 is a prime number`` () =
        selectPrimeNumber 2 |> should equal true

    [<Test>]
    member _.``3 is a prime number`` () =
        selectPrimeNumber 3 |> should equal true

    [<Test>]
    member _.``4 is not a prime number`` () =
        selectPrimeNumber 4 |> should equal false

    [<Test>]
    member _.``5 is a prime number`` () =
        selectPrimeNumber 5 |> should equal true

    [<Test>]
    member _.``Check first few primes`` () =
        let expectedPrimes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
        let actualPrimes = Seq.take 10 allPrimes |> Seq.toList
        actualPrimes |> should equal expectedPrimes