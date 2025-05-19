module BracketSequence 

// A function that checks a string for the correct parenthesis sequence.
let checkBracket (str: string) =
    let rec check ls count =
        if count = str.Length then
            List.isEmpty ls
        else
            let c = str.[count]
            match c with
            | '(' | '[' | '{' -> 
                check (c :: ls) (count + 1)
            | ')' | ']' | '}' ->
                match ls with
                | '('::tail when c = ')' -> check tail (count + 1)
                | '['::tail when c = ']' -> check tail (count + 1)
                | '{'::tail when c = '}' -> check tail (count + 1)
                | _ -> false
            | _ -> 
                check ls (count + 1)

    check [] 0