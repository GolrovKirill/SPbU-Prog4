module Lazy

/// Represents a lazy computation that returns a value of type 'a
type ILazy<'a> =
    abstract Get: unit -> 'a

/// Single-threaded lazy implementation with no synchronization.
type SingleThreadedLazy<'a>(supplier: unit -> 'a) =
    let mutable valueOpt = None
    let mutable supplierRef = Some supplier 

    interface ILazy<'a> with
        member _.Get() =
            match valueOpt with
            | Some value -> value
            | None ->
                let computed = supplierRef.Value()
                valueOpt <- Some computed
                supplierRef <- None
                computed

/// Thread-safe lazy implementation using lock for synchronization.
type MultiThreadedLazy<'a>(supplier: unit -> 'a) =
    [<VolatileField>]
    let mutable valueOpt = None
    let mutable supplierRef = Some supplier 
    let lockObj = obj()

    interface ILazy<'a> with
        member _.Get() =
            match valueOpt with
            | Some value -> value
            | None ->
                lock lockObj (fun () ->
                    match valueOpt with
                    | Some value -> value
                    | None ->
                        let computed = supplierRef.Value()
                        valueOpt <- Some computed
                        supplierRef <- None
                        computed)

/// Lock-free thread-safe lazy implementation using Interlocked.
type LockFreeLazy<'a when 'a: equality>(supplier: unit -> 'a) =
    [<VolatileField>]
    let mutable valueOpt = None
    let mutable supplierRef = Some supplier

    interface ILazy<'a> with
        member _.Get() =
            match valueOpt with
            | Some value -> value
            | None ->
                let computed = supplier()
                if System.Threading.Interlocked.CompareExchange(&valueOpt, Some computed, None) = None then
                    supplierRef <- None
                valueOpt.Value
