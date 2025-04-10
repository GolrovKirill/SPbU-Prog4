
// Create type queue
type PriorityQueue<'T> = {
    Queue: ('T * int) list
}

// Create queue
let createQueue () = { Queue = [] }

// Create method for input element in queue
let inputQueue (value: 'T) (priority: int) (pq: PriorityQueue<'T>) =
    { pq with Queue = (value, priority) :: pq.Queue }

// Create method for otput element in queue
let outputQueue (pq: PriorityQueue<'T>) =
    match pq.Queue with
    | [] -> failwith "Empty queue"
    | _ ->
        let maxPriority = pq.Queue|> List.minBy snd
        let newQueue = pq.Queue |> List.filter ((<>) maxPriority)
        (fst maxPriority, { pq with Queue = newQueue })


// Create sequence from 1 -1 ...
let rec alternatingSequence() =
    seq {
        yield 1
        yield -1
        yield! alternatingSequence()
    }

// Create need sequence
let infiniteSequence() =
    let indices = Seq.initInfinite (fun n -> n + 1)
    Seq.zip indices (alternatingSequence())
    |> Seq.map (fun (n, sign) -> n * sign)
