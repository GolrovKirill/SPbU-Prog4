module LocalNetwork.Tests

open NUnit.Framework
open FsUnit
open LocalNetwork

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestCheckNetworkWithUnreachablePC () =
    let pcs = [
        1, new PC(1, "Windows", true)
        2, new PC(2, "Linux", false)
        3, new PC(3, "Windows", false)
        4, new PC(4, "Mac", false)
    ]

    let graph = [
        1, Set.ofList [2; 3]
        2, Set.ofList [1; 3]
        3, Set.ofList [1; 2]
        4, Set.empty
    ]

    let prob = [
        "Windows", 0.8
        "Linux", 0.5
        "Mac", 1.0
    ]

    let pcsMap = Map pcs
    let graphMap = Map graph
    let probMap = Map prob
    let (reachable, unreachable) = checkNetwork pcsMap graphMap probMap (Set.ofList [1])
    (Set.toList reachable, Set.toList unreachable) |> should equal ([1; 2; 3], [4])

[<Test>]
let TestCheckNetworkWithImpossibleInfected () =
    let pcs = [
        1, new PC(1, "Windows", true)
        2, new PC(2, "Linux", false)
        3, new PC(3, "Windows", false)
        4, new PC(4, "Mac", false)
        5, new PC(5, "Linux", false)
    ]

    let graph = [
        1, Set.ofList [2; 3]
        2, Set.ofList [1; 3]
        3, Set.ofList [1; 2]
        4, Set.ofList [3]
        5, Set.ofList [4]
    ]

    let prob = [
        "Windows", 0.8
        "Linux", 0.5
        "Mac", 0.0
    ]

    let pcsMap = Map pcs
    let graphMap = Map graph
    let probMap = Map prob
    let (reachable, unreachable) = checkNetwork pcsMap graphMap probMap (Set.ofList [1])
    (Set.toList reachable, Set.toList unreachable) |> should equal ([1; 2; 3], [4; 5])

[<Test>]
let TestMoveInfected1 () =
    let pcs = [
        1, new PC(1, "Windows", true)
        2, new PC(2, "Linux", false)
        3, new PC(3, "Windows", false)
        4, new PC(4, "Mac", false)
        5, new PC(5, "Mac", false)
    ]

    let graph = [
        1, Set.ofList [2; 3]
        2, Set.ofList [1; 3; 5]
        3, Set.ofList [1; 2; 4]
        4, Set.ofList [3]
        5, Set.ofList [2]
    ]

    let prob = [
        "Windows", 0.8
        "Linux", 0.5
        "Mac", 0.7
    ]

    let funcRandom = fun () -> 0.0
    let pcsMap = Map pcs
    let graphMap = Map graph
    let probMap = Map prob
    let setIdInfectedPC = Set.ofList [1]
    let result = moveInfected pcsMap graphMap probMap setIdInfectedPC funcRandom

    let expected = [
        Set [1]
        Set [1; 2; 3]
        Set [1; 2; 3; 4; 5]
    ]
    
    List.rev result |> should equal expected

[<Test>]
let TestAllInfectedWithProbabilityOne () =
        let pcs = [
            1, new PC(1, "Windows", true)
            2, new PC(2, "Linux", false)
            3, new PC(3, "Mac", false)
        ]

        let graph = [
            1, Set.ofList [2]
            2, Set.ofList [1; 3]
            3, Set.ofList [2]
        ]

        let prob = [
            "Windows", 1.0
            "Linux", 1.0
            "Mac", 1.0
        ]

        let funcRandom = fun () -> 0.0
        let pcsMap = Map pcs
        let graphMap = Map graph
        let probMap = Map prob
        let infectedStart = Set.ofList [1]
        let progression = moveInfected pcsMap graphMap probMap infectedStart funcRandom

        let expected = [
            Set [1]
            Set [1; 2]
            Set [1; 2; 3]
        ]

        List.rev progression |> should equal expected

[<Test>]
let TestNoInfectionWithProbabilityZero () =
        let pcs = [
            1, new PC(1, "Windows", true)
            2, new PC(2, "Linux", false)
            3, new PC(3, "Mac", false)
        ]

        let graph = [
            1, Set.ofList [2]
            2, Set.ofList [1; 3]
            3, Set.ofList [2]
        ]

        let prob = [
            "Windows", 0.0
            "Linux", 0.0
            "Mac", 0.0
        ]

        let funcRandom = fun () -> 0.0
        let pcsMap = Map pcs
        let graphMap = Map graph
        let probMap = Map prob
        let infectedStart = Set.ofList [1]
        let progression = moveInfected pcsMap graphMap probMap infectedStart funcRandom


        let expected = [
            Set [1]
            Set [1]
        ]

        List.rev progression |> should equal expected

[<Test>]
let TestPartialInfectionWithMixedProbabilities () =
        let pcs = [
            1, new PC(1, "Windows", true)
            2, new PC(2, "Linux", false)
            3, new PC(3, "Mac", false)
            4, new PC(4, "Linux", false)
        ]

        let graph = [
            1, Set.ofList [2]
            2, Set.ofList [1; 3]
            3, Set.ofList [2; 4]
            4, Set.ofList [3]
        ]

        let prob = [
            "Windows", 1.0
            "Linux", 0.4
            "Mac", 0.0
        ]

        let funcRandom = fun () -> 0.3
        let pcsMap = Map pcs
        let graphMap = Map graph
        let probMap = Map prob
        let infectedStart = Set.ofList [1]
        let progression = moveInfected pcsMap graphMap probMap infectedStart funcRandom

        let expected = [
            Set [1]
            Set [1; 2]
        ]

        List.rev progression |> should equal expected

[<EntryPoint>]
let main _ = 0