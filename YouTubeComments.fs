namespace YouTudes

open System.Text.RegularExpressions
open FSharp.Data
open Utility

module YouTubeComments =

    /// Gets a Match's Group value if it succeeded, otherwise None
    let getRegexGroupValue (regex: Regex) (group: string) input =
        match regex.Match input with
        | m when m.Success -> Some m.Groups.[group].Value
        | _ -> None

    let videoIdRegex = Regex(@"/watch\?v=(?<Id>[^&]+)", RegexOptions.Compiled)
    let getVideoId = getRegexGroupValue videoIdRegex "Id"
    let sessionTokenRegex = Regex("'XSRF_TOKEN':\\s+\"(?<Token>.+?)\"", RegexOptions.Compiled)
    let getSessionToken = getRegexGroupValue sessionTokenRegex "Token"

    /// Gets a sequence of comments from videos resulting from a given query
    let getComments query =

        let host = "https://www.youtube.com"

        // perform search
        printfn "Querying search results for %s..." query
        let searchUrl = sprintf "%s/results?search_query=%s" host query
        let cookies = System.Net.CookieContainer() // cookies required for comment JSON requests
        let searchHtml = Http.RequestString(searchUrl, cookieContainer = cookies)
        // parse session token from HTML, required for comment JSON requests
        let sessionToken =
            match getSessionToken searchHtml with
            | Some t -> t
            | None -> failwith "Failed to parse session token from HTML"

        // get links to videos from search results
        let searchDoc = HtmlDocument.Parse searchHtml
        let videoUrls =
            searchDoc.Select("//*[@class='yt-lockup-title']/a[@href]")
            |> Seq.map (fun a -> host + a.AttributeValue("href"))
            |> Seq.where (fun url -> url.Contains("/watch?"))
            |> Seq.distinct

        let requestComments videoUrl jsonUrl =
            let body = FormValues [("session_token", sessionToken); ("client_url", videoUrl)]
            let headers = [("Referer", videoUrl); ("Origin", host)]
            printfn "Requesting comments for %s..." jsonUrl
            Http.RequestString(jsonUrl, httpMethod = "POST", body = body, headers = headers, cookieContainer = cookies)
        
        let getComments videoUrl =
            let getCommentUrl videoId = sprintf "%s/watch_fragments_ajax?v=%s&tr=time&distiller=1&frags=comments&spf=load" host videoId
            match getVideoId videoUrl with
            | Some id ->
                let json = getCommentUrl id |> requestComments videoUrl |> JsonValue.Parse
                maybe {
                    let! body = json.TryGetProperty("body")
                    let! html = body.TryGetProperty("watch-discussion")
                    return html
                }
            | None -> failwithf "Failed to parse video ID from video URL %s" videoUrl

        // parses the embedded HTML from a JSON comment result, returns the comment text
        let parseComments (body: JsonValue) =
            let commentHtml = HtmlDocument.Parse(body.AsString())
            commentHtml.Select("//*[@class='comment-text-content']")
            |> Seq.map HtmlNode.innerText

        // request and parse the JSON comments
        videoUrls |> Seq.choose getComments |> Seq.collect parseComments
