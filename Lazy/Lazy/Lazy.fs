module Lazy

type ILazy<'a> =
    abstract Get: unit -> 'a

type SingleThreadedLazy<'a>(supplier: unit -> 'a) =
    let mutable valueOpt = None
    interface ILazy<'a> with
        member _.Get() =
            match valueOpt with
            | Some value -> value
            | None ->
                let computed = supplier()
                valueOpt <- Some computed
                computed

type MultiThreadedLazy<'a>(supplier: unit -> 'a) =
    let mutable valueOpt = None
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
                        let computed = supplier()
                        valueOpt <- Some computed
                        computed)

type LockFreeLazy<'a>(supplier: unit -> 'a) =
    let mutable valueOpt = None
    interface ILazy<'a> with
        member _.Get() =
            match valueOpt with
            | Some value -> value
            | None ->
                let computed = supplier()
                System.Threading.Interlocked.CompareExchange(&valueOpt, Some computed, None)
                |> ignore
                valueOpt.Value
