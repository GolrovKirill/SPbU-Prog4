/// Function that calculates Fibonacci numbers and accepts integers. 
let fibonacci num = 
    let rec calculate firstNum secondNum count =
            match count with
            | 0 -> firstNum
            | _ -> calculate secondNum (firstNum + secondNum) (count - 1) 
    
    match num with
    | 0 -> 0
    | 1 -> 1
    | num when num < 0 -> 
        match num with
        | num when ((abs num) % 2) <> 0 -> calculate 0 1 (abs num)
        | _ -> -(calculate 0 1 (abs num))
    | _ ->
        calculate 0 1 num

printfn $"{fibonacci -4}"
