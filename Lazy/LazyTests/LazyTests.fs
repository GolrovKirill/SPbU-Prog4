module LazyTests

open Lazy
open NUnit.Framework
open FsUnit
open System.Threading.Tasks

[<Test>]
let ``SingleThreadedLazy should compute value only once`` () =
    let mutable computations = 0
    let lazyVal = SingleThreadedLazy(fun () -> 
        computations <- computations + 1; 3) :> ILazy<int>
    
    lazyVal.Get() |> should equal 3
    lazyVal.Get() |> should equal 3
    computations |> should equal 1

[<Test>]
let ``MultiThreadedLazy should compute once in concurrent environment`` () =
    let mutable computations = 0
    let lazyVal = MultiThreadedLazy(fun () -> 
        computations <- computations + 1; 3) :> ILazy<int>
    
    let results = Array.zeroCreate 100
    Parallel.For(0, 100, fun i ->
        results[i] <- lazyVal.Get()
    ) |> ignore
    
    results |> Array.forall (fun x -> x = 3) |> should be True
    computations |> should equal 1

[<Test>]
let ``LockFreeLazy may compute multiple times but returns consistent results`` () =
    let mutable computations = 0
    let lazyVal = LockFreeLazy(fun () -> 
        System.Threading.Interlocked.Increment(&computations) |> ignore; 3) :> ILazy<int>
    
    let results = Array.zeroCreate 100
    Parallel.For(0, 100, fun i ->
        results[i] <- lazyVal.Get()
    ) |> ignore
    
    results |> Array.forall (fun x -> x = 3) |> should be True
    computations |> should be (greaterThanOrEqualTo 1)
    computations |> should be (lessThanOrEqualTo 100)