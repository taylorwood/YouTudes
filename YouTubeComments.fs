namespace YouTudes

open System
open System.Text.RegularExpressions
open FSharp.Data
open Utility

module YouTubeComments =

    /// Gets a Match's Group value if it succeeded, otherwise None
    let getRegexGroupValue (regex: Regex) (group: string) input =
        match regex.Match input with
        | m when m.Success ->
            let value = m.Groups.[group].Value
            if String.IsNullOrWhiteSpace value then None
            else Some value
        | _ -> None

    let videoIdRegex = Regex(@"/watch\?v=(?<Id>\w+)", RegexOptions.Compiled)
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
        let videoContainers = searchDoc.Descendants (fun n -> n.HasClass("yt-lockup-title"))
        let videoUrls =
            videoContainers
            |> Seq.collect (fun n -> n.Elements("a"))
            |> Seq.choose (fun n -> n.TryGetAttribute("href"))
            |> Seq.map (fun attr -> host + attr.Value())
            |> Seq.where (fun url -> url.Contains("/watch?"))
            |> Seq.distinct

        // requests comments JSON
        let getCommentJson videoUrl jsonUrl =
            let body = FormValues [("session_token", sessionToken); ("client_url", videoUrl)]
            let headers = [("Referer", videoUrl)
                           ("Content-Type", "application/x-www-form-urlencoded")
                           ("DNT", "1")
                           ("Origin", host)]
            printfn "Requesting comments for %s..." jsonUrl
            Http.RequestString(jsonUrl, httpMethod = "POST", body = body, headers = headers, cookieContainer = cookies)
        
        let getCommentUrl videoId = sprintf "%s/watch_fragments_ajax?v=%s&tr=time&distiller=1&frags=comments&spf=load" host videoId

        let getVideoCommentJson videoUrl =
            match getVideoId videoUrl with
            | Some id -> getCommentUrl id |> getCommentJson videoUrl
            | None -> failwithf "Failed to parse video ID from video URL %s" videoUrl

        // request and parse the JSON comments
        let commentBodies =
            videoUrls
            |> Seq.map getVideoCommentJson
            |> Seq.map JsonValue.Parse
            |> Seq.map
                (fun j ->
                    maybe {
                        let! body = j.TryGetProperty("body")
                        let! html = body.TryGetProperty("watch-discussion")
                        return html
                    })
            |> Seq.choose id
        
        // parses the embedded HTML from a JSON comment result, returning the comment text
        let getCommentText (body: JsonValue) =
            let commentHtml = HtmlDocument.Parse(body.AsString())
            commentHtml.Descendants (fun n -> n.HasClass "comment-text-content")
            |> Seq.map HtmlNodeExtensions.InnerText

        commentBodies |> Seq.map getCommentText |> Seq.collect id
