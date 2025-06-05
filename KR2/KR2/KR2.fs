module KR2

open System
open System.Collections.Generic
open System.Threading

/// Function that create Rhombus with length size n.
let createRhombus n =
    let totalLines = 2 * n - 1
    let width = totalLines
    
    let rec buildLine currentPos leftSpaces stars =
        if currentPos >= width then []
        else
            let char = 
                if currentPos >= leftSpaces && currentPos < leftSpaces + stars 
                then '*' 
                else ' '
            char :: buildLine (currentPos + 1) leftSpaces stars
    
    let rec generateLines lineIndex acc =
        if lineIndex >= totalLines then List.rev acc
        else
            let distance = abs (lineIndex - (n - 1))
            let stars = 2 * (n - distance) - 1
            let leftSpaces = distance
            let line = 
                buildLine 0 leftSpaces stars 
                |> List.toArray 
                |> String
            generateLines (lineIndex + 1) (line :: acc)
    
    generateLines 0 []

/// Class implementing a safe queue.
type BlockingQueue<'T>() =
    let queue = Queue<'T>()
    let locker = obj()

    member this.Enqueue(item: 'T) =
        lock locker (fun () ->
            queue.Enqueue(item)
            Monitor.PulseAll locker
        )

    member this.Dequeue() : 'T =
        lock locker (fun () ->
            while queue.Count = 0 do
                Monitor.Wait locker |> ignore
            queue.Dequeue()
        )         
