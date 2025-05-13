module LambdaInterpreter

type Lambda =
    | Var of string
    | Abs of string * Lambda
    | App of Lambda * Lambda

// This function traverses a lambda calculus expression and returns two sorted lists:
// one with all bound variables and another with all free variables found in the expression.
let searchBoundAndFreeVariables exprTerm =
    let rec search exprTerm resultBound resultFree cont =
        match exprTerm with
        | Var(value) when not (List.contains value resultBound)  ->
            cont (resultBound, value :: resultFree)
        | Abs(bound, expr) ->
            search expr (bound :: resultBound) resultFree cont
        | App(expr1, expr2) ->
            search expr1 resultBound resultFree (fun (nextBound, nextFree) ->
                search expr2 nextBound nextFree cont)
        | _ ->
            cont (resultBound, resultFree)
    
    let resultBound, resultFree = search exprTerm [] [] id

    List.sort resultBound, List.sort resultFree

// This function performs alpha-conversion on two lambda calculus expressions.
// It renames bound variables to avoid name collisions between the two expressions
// while preserving the structure and free variables.
let alphaConversion oldExprTerm newExprTerm =
    let (listBoundOld, listFreeOld) = searchBoundAndFreeVariables oldExprTerm
    let (listBoundNew, listFreeNew) = searchBoundAndFreeVariables newExprTerm 

    let used = listBoundOld @ listBoundNew @ listFreeOld @ listFreeNew

    let rec createNewBound value newBound usedNames =
        if List.contains newBound usedNames || List.contains newBound used then
            createNewBound value (newBound + "'") usedNames
        else
            newBound
    
    let rec createNewListNeedReplace value newBound listNeedReplace newListNeedReplace =
        match listNeedReplace with
        | (needReplace, replace) :: tail when needReplace <> value ->
            createNewListNeedReplace value newBound tail ((needReplace, replace) :: newListNeedReplace)
        | (needReplace, replace) :: tail when needReplace = value ->
            createNewListNeedReplace value newBound tail ((value, newBound) :: newListNeedReplace)
        | _ ->
            newListNeedReplace

    let rec checkExpr expr listNeedReplace usedNames cont =
        match expr with
        | App(e1, e2) ->
            checkExpr e1 listNeedReplace usedNames (fun (e1', used1) ->
                checkExpr e2 listNeedReplace used1 (fun (e2', used2) ->
                    cont (App(e1', e2'), used2)))
        | Abs(value, expr) ->
            match List.contains value usedNames with
            | true -> 
                let newName = createNewBound value (value + "'") usedNames
                let newUsed = newName :: usedNames
                let newListNeedReplace =
                    match List.exists (fun (k, _) -> k = value) listNeedReplace with
                    | true ->
                        createNewListNeedReplace value newName listNeedReplace []
                    | false ->
                        (value, newName) :: listNeedReplace
                checkExpr expr newListNeedReplace newUsed (fun (expr', used') ->
                    cont (Abs(newName, expr'), used'))
            | false ->
                let newUsed = value :: usedNames
                checkExpr expr listNeedReplace newUsed (fun (expr', used') ->
                    cont (Abs(value, expr'), used'))
        | Var(value) ->
            match List.tryFind (fun (k, _) -> k = value) listNeedReplace with
            | Some (_, newName) -> cont (Var(newName), usedNames)
            | None -> cont (Var(value), usedNames)

    let renamedExprOld, _ = checkExpr oldExprTerm [] [] id
    let renamedExprNew, _ = checkExpr newExprTerm [] [] id

    let checkBoundInExprs inputExpr listFreeNew listBoundOld = 
        let rec check listFreeNew listBoundOld expr =
            match listFreeNew with
            | head :: tail when List.contains head listBoundOld ->
                let newExpr, _ = checkExpr expr [] [head] id
                check tail listBoundOld newExpr
            | head :: tail when not (List.contains head listBoundOld) ->
                check tail listBoundOld expr
            | _ ->
                expr

        check listFreeNew listBoundOld inputExpr

    let resultExprOld = checkBoundInExprs renamedExprOld listFreeNew listBoundOld 
    let resultExprNew = checkBoundInExprs renamedExprNew listFreeOld listBoundNew

    (resultExprOld, resultExprNew)

// Substitution function: replaces all occurrences of the need Replace variable in the expression with the need paste expression.
let paste exprTerm needReplace needPaste =
    let rec substTerm exprTerm needReplace needPaste =
        match exprTerm with
        | Var(value) ->
            match value with
            | value when value = needReplace ->
                needPaste
            | _ -> 
                Var(value)
        | Abs(value, expr) ->
            let newExpr = substTerm expr needReplace needPaste
            Abs(value, newExpr)
        | App(e1, e2) ->
            let leftExpr = substTerm e1 needReplace needPaste
            let rightExpr = substTerm e2 needReplace needPaste
            App(leftExpr, rightExpr)

    substTerm exprTerm needReplace needPaste

// A function that performs beta reduction of a lambda expression.
// Consistently applies the reduction rules, reducing the expression to a simpler form.
let betaReduction exprTerm =
    let rec beta exprTerm =
        match exprTerm with
        | App(e1, e2) ->
            match (e1, e2) with
            | Var(value1), Var(vale2) -> 
                exprTerm
            | Var(value1), Abs(value2, expr) -> 
                exprTerm
            | Var(value), App(e1', e2') ->
                App(Var(value), beta (App(beta e1', beta e2')))
            | Abs(value1, expr), _ ->
                let checkLeft, checkRight = alphaConversion e1 e2
                match checkLeft with
                | Abs(v, e) ->
                    beta (paste e v checkRight)
                | _ -> 
                    exprTerm
                
            | App(e1', e2'), _ ->
                beta (App((beta (App(e1', e2'))), beta e2))
        | Abs(value, expr) -> 
            Abs(value, beta expr)
        | _ ->
            exprTerm
    
    beta exprTerm