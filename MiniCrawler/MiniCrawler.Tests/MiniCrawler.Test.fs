module MiniCrawler.Tests

open NUnit.Framework
open MiniCrawler
open FsUnit

[<TestFixture>]
type WebPageProcessorTests() =
    
    [<Test>]
    member this.``ExtractLinks should return absolute links from HTML content``() =
        let html = """
            <html>
                <a href="https://example.com/page1">Page1</a>
                <a href="http://example.com/page2">Page2</a>
                <a href="/page3">Invalid</a>
            <html>
        """
        
        let links = WebPageProcessor.extractLinks html
        
        links |> should contain "https://example.com/page1"
        links |> should contain "http://example.com/page2"
        links |> should not' (contain "/page3")
    
    [<Test>]
    member this.``ExtractLinks should handle GitHub homepage links correctly``() =
        async {
            let! content = WebPageProcessor.downloadContent "https://github.com"
            let links = WebPageProcessor.extractLinks content
            
            links.Length |> should equal 61
        } |> Async.RunSynchronously
    
    [<Test>]
    member this.``ProcessSinglePage should successfully process example.com``() =
        async {
            do! WebPageProcessor.processSinglePage "https://example.com"
        } |> Async.StartAsTask :> System.Threading.Tasks.Task

[<EntryPoint>]
let main _ = 0
