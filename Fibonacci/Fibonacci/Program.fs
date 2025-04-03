let rec fibonacci lastNum = 
    if lastNum = 0 then
        0
    elif lastNum = 1 then
        1
    else
        let rec fib firstNum secondNum count =
            if count = 0 then firstNum
            else fib secondNum (firstNum + secondNum) (count - 1)
        
        fib 0 1 lastNum

printfn $"{fibonacci 0}"
