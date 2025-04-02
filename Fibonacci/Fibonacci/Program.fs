let rec fibonacci lastNum firstNum secondNum = 
    if lastNum = 0 then
        firstNum
    elif lastNum = 1 then
        secondNum
    else
        let nextNum = firstNum + secondNum
        fibonacci 
            (lastNum - 1)
            secondNum
            nextNum

printfn $"{fibonacci 100 0 1}"
