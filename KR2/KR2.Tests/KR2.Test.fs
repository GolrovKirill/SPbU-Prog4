module KR2.Tests

open NUnit.Framework
open FsUnit
open System.Threading
open System
open KR2

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``TestCreateRhombusWith4``() =
    let result = createRhombus 4

    let expected = [
        "   *   ";
        "  ***  ";
        " ***** ";
        "*******";
        " ***** ";
        "  ***  ";
        "   *   "
    ]

    result |> should equal expected

[<Test>]
let ``TestCreateRhombusWith5``() =
    let result = createRhombus 5

    let expected = [
        "    *    ";
        "   ***   ";
        "  *****  ";
        " ******* ";
        "*********";
        " ******* ";
        "  *****  ";
        "   ***   ";
        "    *    "
    ]

    result |> should equal expected


[<Test>]
let ``BlockingQueue should enqueue and dequeue correctly``() =
    let queue = BlockingQueue<int>()
    let result = ref 0
    let thread = Thread(ThreadStart(fun () -> 
        result := queue.Dequeue()
    ))
    thread.Start()

    Thread.Sleep(200)
    
    !result |> should equal 0

    queue.Enqueue(42)
    
    thread.Join(2000) |> should be True
    
    !result |> should equal 42

[<Test>]
let ``BlockingQueue should work with multiple items``() =
    let queue = BlockingQueue<string>()
    queue.Enqueue("a")
    queue.Enqueue("b")
    queue.Enqueue("c")

    queue.Dequeue() |> should equal "a"
    queue.Dequeue() |> should equal "b"
    queue.Dequeue() |> should equal "c"

[<EntryPoint>]
let main _ = 0
