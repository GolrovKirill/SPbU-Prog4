module LazyTests

open Lazy
open NUnit.Framework
open FsUnit
open System.Threading
open System.Threading.Tasks

[<Test>]
let ``SingleThreadedLazy should compute value only once`` () =
    let computations = ref 0
    let lazyVal = SingleThreadedLazy(fun () -> 
        Interlocked.Increment(computations) |> ignore
        System.Object()) :> ILazy<obj>
    
    let first = lazyVal.Get()
    let second = lazyVal.Get()
    System.Object.ReferenceEquals(first, second) |> should be True
    !computations |> should equal 1

[<Test>]
let ``MultiThreadedLazy should compute once in concurrent environment`` () =
    let computations = ref 0
    let lazyVal = MultiThreadedLazy(fun () -> 
        Interlocked.Increment(computations) |> ignore
        System.Object()) :> ILazy<obj>
    
    let results = Array.init 100 (fun _ -> null)
    Parallel.For(0, 100, fun i ->
        results[i] <- lazyVal.Get()
    ) |> ignore
    
    let first = results.[0]
    results 
    |> Array.forall (fun x -> System.Object.ReferenceEquals(x, first)) 
    |> should be True
    !computations |> should equal 1

[<Test>]
let ``LockFreeLazy may compute multiple times but returns consistent results`` () =
    let computations = ref 0
    let lazyVal = LockFreeLazy(fun () -> 
        Interlocked.Increment(computations) |> ignore
        System.Object()) :> ILazy<obj>
    
    let results = Array.init 100 (fun _ -> null)
    Parallel.For(0, 100, fun i ->
        results[i] <- lazyVal.Get()
    ) |> ignore
    
    let first = results.[0]
    results 
    |> Array.forall (fun x -> System.Object.ReferenceEquals(x, first)) 
    |> should be True
    !computations |> should be (greaterThanOrEqualTo 1)