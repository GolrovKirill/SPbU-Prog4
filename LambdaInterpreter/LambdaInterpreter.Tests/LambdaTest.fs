module LambdaTest

open NUnit.Framework
open FsUnit
open LambdaInterpreter

[<TestFixture>]
type SearchBoundAndFreeVariablesTests () =
    [<Test>]
    member _.``Test1 searchBoundAndFreeVariables``  () =
        let input = Abs("x", Var("y"))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"]

    [<Test>]
    member _.``Test2 searchBoundAndFreeVariables`` () =
        let input = Abs("x", Var("x"))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"]

    [<Test>]
    member _.``Test3 searchBoundAndFreeVariables`` () =
        let input = Abs("x", Abs("x", Var("x")))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"; "x"]

    [<Test>]
    member _.``Test4 searchBoundAndFreeVariables`` () =
        let input = Var("y")
        let (listBound, listFree) = searchBoundAndFreeVariables input
        listBound |> should equal ([] : string list)

    [<Test>]
    member _.``Test5 searchBoundAndFreeVariables`` () =
        let input = App(Var("y"), Var("z"))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        listBound |> should equal ([] : string list)

    [<Test>]
    member _.``Test6 searchBoundAndFreeVariables`` () =
        let input = App(Abs("x", Var("y")), Abs("x", Var("z")))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"; "x"]

    [<Test>]
    member _.``Test7 searchBoundAndFreeVariables`` () =
        let input = Abs("z", App(Abs("x", Var("y")), Abs("x", Var("z"))))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"; "x"; "z"]

    [<Test>]
    member _.``Test8 searchBoundAndFreeVariables`` () =
        let input = App(Abs("x", App(Var("x"), Var("y"))), Abs("y", Var("z")))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"; "y"]

    [<Test>]
    member _.``Test9 searchBoundAndFreeVariables`` () =
        let input = Abs("x", App(Var("x"), Abs("y", App(Var("x"), Var("y")))))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"; "y"]

    [<Test>]
    member _.``Test10 searchBoundAndFreeVariables`` () =
        let input = App(Abs("x", App(Var("x"), Var("x"))), App(Var("x"), Var("y")))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"]

    [<Test>]
    member _.``Test11 searchBoundAndFreeVariables`` () =
        let input = Abs("x", App(Var("x"), App(Var("y"), Var("x"))))
        let (listBound, listFree) = searchBoundAndFreeVariables input
        List.sort listBound |> should equal ["x"]

[<TestFixture>]
type AlphaConversionTests() =
    [<Test>]
    member _.``Test1: Rename nested duplicate bound variables`` () =
        let oldExpr = Abs("x", Abs("x", Var("x")))
        let newExpr = Var("y")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("x'", Var("x'"))), Var("y"))

    [<Test>]
    member _.``Test2: Multiple nested duplicate variables renamed incrementally`` () =
        let oldExpr = Abs("x", Abs("x", Abs("x", Var("x"))))
        let newExpr = Var("y")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("x'", Abs("x''", Var("x''")))), Var("y"))

    [<Test>]
    member _.``Test3: Applications with shadowed variables renamed`` () =
        let oldExpr = Abs("x", App(Abs("x", Var("x")), Abs("x", Var("x"))))
        let newExpr = Var("y")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", App(Abs("x'", Var("x'")), Abs("x''", Var("x''")))), Var("y"))

    [<Test>]
    member _.``Test4: Normalize inner bound variable without conflict`` () =
        let oldExpr = Abs("x", Abs("x", Var("z")))
        let newExpr = Var("z")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("x'", Var("z"))), Var("z"))

    [<Test>]
    member _.``Test5: No renaming when no variable conflicts`` () =
        let oldExpr = Abs("x", Abs("y", Var("x")))
        let newExpr = Var("z")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("y", Var("x"))), Var("z"))

    [<Test>]
    member _.``Test6: Partial renaming in nested scopes`` () =
        let oldExpr = Abs("x", Abs("x", Abs("y", Var("x"))))
        let newExpr = Var("w")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("x'", Abs("y", Var("x'")))), Var("w"))

    [<Test>]
    member _.``Test7: Rename when free variable matches bound variable`` () =
        let oldExpr = Abs("x", App(Var("x"), Var("x")))
        let newExpr = Var("x")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x'", App(Var("x'"), Var("x'"))), Var("x"))

    [<Test>]
    member _.``Test8: Handle variables with existing apostrophes`` () =
        let oldExpr = Abs("x'", Abs("x'", Var("x'")))
        let newExpr = Var("y")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x'", Abs("x''", Var("x''"))), Var("y"))

    [<Test>]
    member _.``Test9: Multiple clashes with free variables in newExpr`` () =
        let oldExpr = Abs("x", Abs("y", Var("x")))
        let newExpr = App(Var("x"), Var("y"))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x'", Abs("y'", Var("x'"))), App(Var("x"), Var("y")))

    [<Test>]
    member _.``Test10: Closed newExpr triggers normalization`` () =
        let oldExpr = Abs("x", Abs("x", Var("x")))
        let newExpr = Abs("y", Var("y"))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Abs("x'", Var("x'"))), Abs("y", Var("y")))

    [<Test>]
    member _.``Test11: Rename inner bound variable clashing with newExpr`` () =
        let oldExpr = Abs("a", Abs("b", Var("a")))
        let newExpr = Var("b")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("a", Abs("b'", Var("a"))), Var("b"))

    [<Test>]
    member _.``Test12: Rename in complex application structure`` () =
        let oldExpr = App(Abs("x", Var("x")), Abs("y", App(Var("y"), Var("x"))))
        let newExpr = Var("x")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (App(Abs("x'", Var("x'")), Abs("y", App(Var("y"), Var("x")))), Var("x"))

    [<Test>]
    member _.``Test13: Nested conflicts in application`` () =
        let oldExpr = Abs("x", App(Abs("x", Var("x")), Var("x")))
        let newExpr = Var("x")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x'", App(Abs("x''", Var("x''")), Var("x'"))), Var("x"))

    [<Test>]
    member _.``Test14: Conflict across nested abstraction levels`` () =
        let oldExpr = Abs("x", Abs("y", Abs("x", Var("y"))))
        let newExpr = App(Var("x"), Var("y"))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x'", Abs("y'", Abs("x''", Var("y'")))), App(Var("x"), Var("y")))

    [<Test>]
    member _.``Test15: newExpr with bound variables causes no conflict`` () =
        let oldExpr = Abs("x", Var("x"))
        let newExpr = Abs("x", Var("x"))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x", Var("x")), Abs("x", Var("x")))

    [<Test>]
    member _.``Test16: Variables with numbers get renamed`` () =
        let oldExpr = Abs("x1", Abs("x1", Var("x1")))
        let newExpr = Var("x1")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x1'", Abs("x1''", Var("x1''"))), Var("x1"))

    [<Test>]
    member _.``Test17: Deep nesting without conflicts`` () =
        let oldExpr = Abs("a", Abs("a", Abs("a", Var("a"))))
        let newExpr = Var("b")
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("a", Abs("a'", Abs("a''", Var("a''")))), Var("b"))

    [<Test>]
    member _.``Test18: Check correct replace`` () =
        let oldExpr = Abs("x", Abs("x", Abs("x", Var("x"))))
        let newExpr = App(Var("x"), Var("x'"))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (Abs("x''", Abs("x'''", Abs("x''''", Var("x''''")))), App(Var("x"), Var("x'")))

    [<Test>]
    member _.``Test19: Check correct replace to the second argument`` () =
        let oldExpr = App(Var("x"), Var("x'"))
        let newExpr = Abs("x", Abs("x", Abs("x", Var("x"))))
        let result = alphaConversion oldExpr newExpr
        result |> should equal (App(Var("x"), Var("x'")), Abs("x''", Abs("x'''", Abs("x''''", Var("x''''")))))

[<TestFixture>]
type PasteTests () =
    [<Test>]
    member _.``Test1: No substitution when variable is not present`` () =
        let expr = Var("y")
        let needReplace = "x"
        let newExpr = Var("z")
        let result = paste expr needReplace newExpr
        result |> should equal (Var("y"))

    [<Test>]
    member _.``Test2: No substitution with complex replacement when variable absent`` () =
        let expr = Var("y")
        let needReplace = "x"
        let newExpr = App(Var("z"), Var("z"))
        let result = paste expr needReplace newExpr
        result |> should equal (Var("y"))

    [<Test>]
    member _.``Test3: Substitute free variable with application`` () =
        let expr = Var("x")
        let needReplace = "x"
        let newExpr = App(Var("z"), Var("z"))
        let result = paste expr needReplace newExpr
        result |> should equal (App(Var("z"), Var("z")))

    [<Test>]
    member _.``Test4: Substitute free variable preserving bound variable`` () =
        let expr = App(Abs("x'", Var("x'")), Var("x"))
        let needReplace = "x"
        let newExpr = Abs("z", Var("z"))
        let result = paste expr needReplace newExpr
        result |> should equal (App(Abs("x'", Var("x'")), Abs("z", Var("z"))))

    [<Test>]
    member _.``Test5: Substitute free variable inside abstraction body`` () =
        let expr = Abs("y", App(Var("x"), Var("y")))
        let needReplace = "x"
        let newExpr = Var("z")
        let result = paste expr needReplace newExpr
        result |> should equal (Abs("y", App(Var("z"), Var("y"))))

    [<Test>]
    member _.``Test6: Substitute in nested abstraction without capture`` () =
        let expr = Abs("y", App(Var("x"), Var("y")))
        let needReplace = "x"
        let newExpr = Var("z")
        let result = paste expr needReplace newExpr
        result |> should equal (Abs("y", App(Var("z"), Var("y"))))

    [<Test>]
    member _.``Test7: Substitute free variable in application components`` () =
        let expr = App(Var("x"), Abs("y", Var("x")))
        let needReplace = "x"
        let newExpr = Var("z")
        let result = paste expr needReplace newExpr
        result |> should equal (App(Var("z"), Abs("y", Var("z"))))

[<TestFixture>]
type BetaReductionTests () =
   [<Test>]
    member _.``Test1: Beta reduction of single variable remains unchanged`` () =
        let expr = Var("y")
        let result = betaReduction expr 
        result |> should equal (Var("y"))
    
    [<Test>]
    member _.``Test2: Beta reduction applies identity function to abstraction`` () =
        let expr = App(Abs("x", Var("x")), Abs("x", App(Var("x"), Var("x"))))
        let result = betaReduction expr 
        result |> should equal (Abs("x", App(Var("x"), Var("x")))) 

    [<Test>]
    member _.``Test3: Beta reduction reduces identity application to argument`` () =
        let expr = App(Abs("x", Var("x")), Var("y"))
        let result = betaReduction expr
        result |> should equal (Var("y"))

    [<Test>]
    member _.``Test4: Beta reduction reduces self-application of identity function`` () =
        let expr = App(Abs("x", App(Var("x"), Var("x"))), Abs("y", Var("y")))
        let result = betaReduction expr
        result |> should equal (Abs("y", Var("y")))

    [<Test>]
    member _.``Test5: Beta reduction reduces inner identity application in abstraction`` () =
        let expr = Abs("x", App(Abs("y", Var("y")), Var("x")))
        let result = betaReduction expr
        result |> should equal (Abs("x", Var("x")))

    [<Test>]
    member _.``Test6: Beta reduction leaves unbound variable unchanged`` () =
        let expr = Var("z")
        let result = betaReduction expr
        result |> should equal (Var("z"))

    [<Test>]
    member _.``Test7: Beta reduction reduces multi-level nested applications`` () =
        let expr = App(Abs("x", App(Abs("y", App(Var("x"), Var("y"))), Var("z"))), Var("w"))
        let result = betaReduction expr
        result |> should equal (App(Var("w"), Var("z")))

    [<Test>]
    member _.``Test8: Beta reduction ignores redexes inside abstraction unless exposed`` () =
        let expr = Abs("x", App(Var("x"), App(Abs("y", Var("y")), Var("z"))))
        let result = betaReduction expr
        result |> should equal (Abs("x", App(Var("x"), Var("z"))))

    [<Test>]
    member _.``Test9: Beta reduction performs alpha conversion to avoid capture`` () =
        let expr = App(Abs("x", Abs("z", App(Var("x"), Var("z")))), Var("z"))
        let result = betaReduction expr
        result |> should equal (Abs("z'", App(Var("z"), Var("z'"))))

    [<Test>]
    member _.``Test10: Beta reduction reduces leftmost outermost redex first`` () =
        let expr = App(App(Abs("x", Var("x")), Abs("y", Var("y"))), Var("z"))
        let result = betaReduction expr
        result |> should equal (Var("z"))

    [<Test>]
    member _.``Test11: Beta reduction leaves normal forms unchanged`` () =
        let expr = Abs("x", Var("x"))
        let result = betaReduction expr
        result |> should equal (Abs("x", Var("x")))

    [<Test>]
    member _.``Test12: Beta reduction handles nested redexes in order`` () =
        let expr = App(Abs("a", App(Abs("b", Var("a")), Var("c"))), Var("d"))
        let result = betaReduction expr
        result |> should equal (Var("d"))
  
[<EntryPoint>]
let private main _ = 0
