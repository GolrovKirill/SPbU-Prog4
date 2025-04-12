open NUnit.Framework
open EvenNumber
open FsUnit
open FsCheck

module EvenNumberTests = 
  
  [<Test>]
  let ``Equal countEvenNumberWithFilter and countEvenNumberWithMap`` () =
      let inputList = [1; 2; 3; 4; 5; 6]
      let resultFilter = countEvenNumberWithFilter inputList
      let resultMap = countEvenNumberWithMap inputList
      resultFilter |> should equal resultMap

  [<Test>]
  let ``Equal countEvenNumberWithFilter and countEvenNumberWithFold`` () =
      let inputList = [1; 2; 3; 4; 5; 6]
      let resultFilter = countEvenNumberWithFilter inputList
      let resultFold = countEvenNumberWithFold inputList
      resultFilter |> should equal resultFold

  [<Test>]
  let ``Equal countEvenNumberWithFold and countEvenNumberWithMap`` () =
      let inputList = [1; 2; 3; 4; 5; 6]
      let resultFold = countEvenNumberWithFold inputList
      let resultMap = countEvenNumberWithMap inputList
      resultFold |> should equal resultMap

  [<Test>]
  let ``Test use FsCheck for countEvenNumberWithFilter and countEvenNumberWithMap and countEvenNumberWithFold`` () =
    let test (ls: list<int>) = 
        let resultFilter = countEvenNumberWithFilter ls
        let resultFold = countEvenNumberWithFold ls
        let resultMap = countEvenNumberWithMap ls

        resultFilter = resultFold && resultFold = resultMap
    Check.QuickThrowOnFailure test