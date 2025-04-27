module ParsingTree

type ParseTree =
    | EmptyNode
    | Number of value: float
    | Symbol of symbol: string * left: ParseTree * right: ParseTree

// Check what entered parse tree.
let checkTree tree =
    match tree with
    | EmptyNode -> failwith "Entered empty tree."
    | Number(_) -> failwith "Parse tree doesn't start with a number."
    | Symbol(_, _, _) -> true

// Calculate the value of the parse tree use CPS with linearization.
let calculateParseTreeCPS tree =
    checkTree tree |> ignore
    
    let rec createList tree acc cont =
        match tree with
        | Symbol(sym, left, right) ->
            createList left acc (fun leftAcc ->
                createList right leftAcc (fun rightAcc ->
                    cont (sym :: rightAcc)))
        | Number(v) ->
            cont (string(v) :: acc)

    let listTree = List.rev(createList tree [] (fun x -> x))

    let rec calculate (listTree: List<string>) (accNum: List<float>) cont =
        
        match listTree with
        | head :: tail ->
            match System.Double.TryParse(head) with
            | (true, value) ->
                calculate tail (value :: accNum) cont
            | (false, _) when (List.contains head ["+"; "-"; "*"; "/"]) ->
                if List.length accNum < 2 then
                    failwithf "Not enough operands for operator '%s'." head
                else
                    let head1 = accNum.Tail.Head
                    let head2 = accNum.Head
                    match head with 
                    | "+" -> 
                        calculate tail ((head1 + head2) :: accNum.Tail.Tail) cont
                    | "-" -> 
                        calculate tail ((head1 - head2) :: accNum.Tail.Tail) cont
                    | "*" -> 
                        calculate tail ((head1 * head2) :: accNum.Tail.Tail) cont
                    | "/" -> 
                        if head2 = 0.0 then 
                            failwith "Division by 0."
                        else 
                            calculate tail ((head1 / head2) :: accNum.Tail.Tail) cont
                    | _ -> failwith "Entered incorrect operation."         
            | _ -> failwith "Invalid token in expression."
        | [] ->
            match accNum with
            | [result] -> cont result
            | [] -> failwith "Empty expression."
            | _ -> failwith "Expression could not be fully evaluated."

    calculate listTree [] (fun x -> x)
    
// Calculate the value of the parse tree.
let calculateParseTree tree =
    checkTree tree |> ignore
    
    let rec calculateTree node =
        match node with
        | Number(value) -> value
        | Symbol(symbol, left, right) ->
            let leftValue = calculateTree left
            let rightValue = calculateTree right
        
            match symbol with
            | "+" -> leftValue + rightValue
            | "-" -> leftValue - rightValue
            | "*" -> leftValue * rightValue
            | "/" -> 
                if rightValue = 0.0 then failwith "Division by 0."
                else leftValue / rightValue
            | _ -> failwith "Entered incorrect operation."

    calculateTree tree