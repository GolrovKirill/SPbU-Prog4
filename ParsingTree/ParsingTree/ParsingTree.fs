module ParsingTree

type ParseTree =
    | EmptyNode
    | Number of value: float
    | Symbol of symbol: string * left: ParseTree * right: ParseTree

// Calculate the value of the parse tree.
let calculateParseTree tree =
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

    match tree with
    | EmptyNode -> failwith "Entered empty tree."
    | _ ->
        match tree with
        | Number(_) -> failwith "Parse tree doesn't start with a number."
        | Symbol(_, _, _) -> calculateTree tree