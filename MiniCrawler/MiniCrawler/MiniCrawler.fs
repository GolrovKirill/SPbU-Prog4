namespace MiniCrawler

open System
open System.Net.Http
open System.Text.RegularExpressions

module WebPageProcessor =
    let private httpClient = new HttpClient()
    
    /// Downloads content from specified URL asynchronously
    let downloadContent (url: string) = async {
        let! content = httpClient.GetStringAsync(url) |> Async.AwaitTask
        return content
    }
    
    /// Extracts absolute HTTP/HTTPS links from HTML content
    let extractLinks (content: string) =
        let regex = Regex(@"<a\s+[^>]*href=""(http[^""]*)""[^>]*>", RegexOptions.IgnoreCase)
        regex.Matches(content)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups[1].Value)
        |> Seq.distinct
        |> Seq.toList
    
    /// Processes a single URL and prints its information
    let processSinglePage (url: string) = async {
        let! content = downloadContent url
        printfn "%s — %d" url content.Length
    }
    
    /// Main function to process a page and all linked pages
    let processPage (url: string) = async {
        let! mainContent = downloadContent url
        let links = extractLinks mainContent
        let! _ = links |> List.map processSinglePage |> Async.Parallel
        ()
    }