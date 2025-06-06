module Workflow

    type CalculateBuilder() =
        member _.Bind(str: string, f: int -> 'a option) = 
            match System.Int32.TryParse str with
            | (true, num) -> f num
            | _ -> None

        member _.Return(x: 'a) = Some x

    let builder = CalculateBuilder()

    type RoundingBuilder(precision: int) =
        member _.Bind(value: float, f: float -> float) : float =
            let rounded = System.Math.Round(value, precision)
            f rounded

        member _.Return(value: float) : float =
            System.Math.Round(value, precision)

    let rounding precision = RoundingBuilder(precision)