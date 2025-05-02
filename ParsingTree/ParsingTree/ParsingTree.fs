module ParsingTree

type ParseTree =
    | EmptyNode
    | Number of value: float
    | Symbol of symbol: string * left: ParseTree * right: ParseTree

/// Function that checks the input parse tree.
let checkTree tree =
    match tree with
    | EmptyNode -> false
    | Number(_) -> false
    | Symbol(_, _, _) -> true

/// Function that calculates the input parse tree using CPS.
let calculateParseTreeCPS tree =
    if not (checkTree tree) then None else

    let rec calculateTreeCPS node cont =
        match node with
        | EmptyNode -> cont None
        | Number value -> cont (Some value)
        | Symbol(symbol, left, right) ->
            calculateTreeCPS left (fun leftValueOption ->
                match leftValueOption with
                | None -> cont None
                | Some leftValue ->
                    calculateTreeCPS right (fun rightValueOption ->
                        match rightValueOption with
                        | None -> cont None
                        | Some rightValue ->
                            match symbol with
                            | "+" -> cont (Some (leftValue + rightValue))
                            | "-" -> cont (Some (leftValue - rightValue))
                            | "*" -> cont (Some (leftValue * rightValue))
                            | "/" ->
                                if rightValue = 0.0 then cont None
                                else cont (Some (leftValue / rightValue))
                            | _ -> cont None))

    calculateTreeCPS tree id

/// Function that calculates the input parse tree.
let calculateParseTree tree =
    if not (checkTree tree) then None else
    
    let rec calculateTree node =
        match node with
        | EmptyNode -> None
        | Number(value) -> Some value
        | Symbol(symbol, left, right) ->
            let leftValue = calculateTree left
            let rightValue = calculateTree right
        
            match (symbol, leftValue, rightValue) with
            | ("+", Some leftValue, Some rightValue) -> Some (leftValue + rightValue)
            | ("-", Some leftValue, Some rightValue) -> Some (leftValue - rightValue)
            | ("*", Some leftValue, Some rightValue) -> Some (leftValue * rightValue)
            | ("/", Some leftValue, Some rightValue) -> 
                if rightValue = 0.0 then None
                else Some (leftValue / rightValue)
            | _ -> None

    calculateTree tree