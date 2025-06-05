module LocalNetwork
    open System.Collections.Generic
    
    type PC(number: int, os: string, infected: bool) =
        member this.Number = number               
        member this.OS = os
        member this.Infected = infected

        member this.WithInfected(newInfected: bool) = PC(number, os, newInfected)

        member this.getProbabilityInfectedPC (probabilityRule: Map<string, float>) = 
            match Map.tryFind this.OS probabilityRule with
            | Some value -> value
            | None -> 0.0

        interface System.IComparable with
            member this.CompareTo obj =
                match obj with
                | :? PC as other -> compare this.Number other.Number
                | _ -> invalidArg "obj" "Not a PC instance"
        override this.Equals(obj) =
            match obj with
            | :? PC as other -> this.Number = other.Number
            | _ -> false
        override this.GetHashCode() = hash this.Number
    
    // Function that return new infected pc.
    let createNewInfectedPC (pc: PC) =
        pc.WithInfected(true)
    
    // Function that check input data on correct.
    let validateNetwork (mapPC: Map<int, PC>) (graph: Map<int, Set<int>>) (infectedIds: Set<int>) =
        
        let existingIds = mapPC |> Map.keys |> Set.ofSeq
        let missingInfected = Set.difference infectedIds existingIds
        
        if not missingInfected.IsEmpty then
            false
        else   
            let graphVertices = graph |> Map.keys |> Set.ofSeq
            let missingVertices = Set.difference graphVertices existingIds
            
            if not missingVertices.IsEmpty then
                false
            else
                let allNeighbors = 
                    graph 
                    |> Map.values 
                    |> Set.unionMany    
                let missingNeighbors = Set.difference allNeighbors existingIds
                
                if not missingNeighbors.IsEmpty then
                    false
                else
                    true

    // Function that return two lists when first contains pcs which possible infected second another pcs.
    let checkNetwork (mapPC: Map<int, PC>) (graph: Map<int, Set<int>>) 
        (probMap: Map<string, float>) (infectedIds: Set<int>) =

        let rec bfs (visited: Set<int>) (queue: Queue<int>) =
            if queue.Count = 0 then visited
            else
                let current = queue.Dequeue()
                if visited.Contains(current) then bfs visited queue
                else
                    let newVisited = Set.add current visited
                    let neighbors = 
                        match Map.tryFind current graph with
                        | Some(ns) -> 
                            ns |> Set.filter (fun n ->
                                not (newVisited.Contains n) &&
                                match Map.tryFind n mapPC with
                                | Some pc -> 
                                    pc.getProbabilityInfectedPC(probMap) > 0.0 
                                    && not pc.Infected
                                | None -> false)
                        | None -> Set.empty
                
                    neighbors |> Set.iter (fun n -> queue.Enqueue(n))
                    bfs newVisited queue
    
        let queue = Queue<int>(Seq.toList infectedIds)
        let reachable = bfs Set.empty queue
        let allPCs = mapPC |> Map.keys |> Set.ofSeq
        (reachable, Set.difference allPCs reachable)     

    // Function that return lists infected PC in every step. 
    let moveInfected (mapPC: Map<int, PC>) (graph: Map<int, Set<int>>) 
                (probMap: Map<string, float>) (infectedIds: Set<int>)
                (random: unit -> float) = 
    
        if not (validateNetwork mapPC graph infectedIds) then []
        else
            let (reachable, unreachable) = checkNetwork mapPC graph probMap infectedIds
            let unreachableSet = Set.ofSeq unreachable

            let rec simulate (state: Map<int, PC>) 
                            (infected: Set<int>)
                            (history: list<Set<int>>)
                            (steps: int) =

                let candidates = 
                    infected
                    |> Set.toSeq
                    |> Seq.choose (fun id -> Map.tryFind id graph)
                    |> Set.unionMany  
                    |> Set.filter (fun id ->
                        not (unreachableSet.Contains id) &&
                        not (infected.Contains id))
            
                let newInfected =
                    candidates
                    |> Set.filter (fun id ->
                        match Map.tryFind id state with
                        | Some pc when not pc.Infected ->
                            random() <= pc.getProbabilityInfectedPC(probMap)
                        | _ -> false)
            
                let newState = 
                    newInfected
                    |> Set.fold (fun acc id -> 
                        Map.change id (function 
                            | Some pc -> Some (createNewInfectedPC pc) 
                            | None -> None) acc) state
            
                let allInfected = Set.union infected newInfected
                let newHistory = allInfected :: history
            
                if Set.count allInfected >= Set.count reachable ||
                    steps > 10000 then
                        newHistory
                else 
                    simulate newState allInfected newHistory (steps + 1)
        
            simulate mapPC infectedIds [infectedIds] 1

