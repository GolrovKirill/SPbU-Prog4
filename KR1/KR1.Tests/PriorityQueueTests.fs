open NUnit.Framework
open System

module PriorityQueueTests =
    
    open KR1.Program

    [<TestFixture>]
    type PriorityQueueTests() =

        [<Test>]
        member this.``Test createQueue creates an empty queue``() =
            let pq = createQueue()
            Assert.AreEqual([], pq.Queue)

        [<Test>]
        member this.``Test enqueue adds an element to the queue``() =
            let pq = createQueue()
            let pq' = enqueue 10 1 pq
            Assert.AreEqual([(10, 1)], pq'.Queue)

        [<Test>]
        member this.``Test outputQueue returns the highest priority element``() =
            let pq = enqueue 10 1 (enqueue 20 2 (enqueue 30 0 (createQueue())))
            let (value, pq') = outputQueue pq
            Assert.AreEqual(30, value) 
            Assert.AreEqual([(10, 1); (20, 2)], pq'.Queue)

