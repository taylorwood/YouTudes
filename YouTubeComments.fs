﻿namespace YouTudes

open System
open System.Text.RegularExpressions
open FSharp.Data

module YouTubeComments =

    /// Gets a Match's Group value if it succeeded, otherwise an empty string
    let getRegexGroupValue (regex: Regex) (group: string) input =
        match regex.Match input with
        | m when m.Success -> m.Groups.[group].Value
        | _ -> String.Empty

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
        let searchHtml = HtmlDocument.Load searchUrl

        // get links to videos from search results
        let videoContainers = searchHtml.Descendants (fun n -> n.HasClass("yt-lockup-title"))
        let videoUrls =
            videoContainers
            |> Seq.collect (fun n -> n.Elements("a"))
            |> Seq.choose (fun n -> n.TryGetAttribute("href"))
            |> Seq.map (fun attr -> host + attr.Value())

        let cookies = System.Net.CookieContainer() // required for comment JSON requests

        let getCommentUrl videoId = sprintf "%s/watch_fragments_ajax?v=%s&tr=time&distiller=1&frags=comments&spf=load" host videoId

        // requests comments JSON
        let getCommentJson token videoUrl jsonUrl =
            let body = FormValues [("session_token", token); ("client_url", videoUrl)]
            let headers = [("Referer", videoUrl)
                           ("Content-Type", "application/x-www-form-urlencoded")
                           ("DNT", "1")
                           ("Origin", host)]
            printfn "Requesting comments for %s..." jsonUrl
            Http.RequestString(jsonUrl, httpMethod = "POST", body = body, headers = headers, cookieContainer = cookies)
        
        let getVideoCommentJson videoUrl =
            printfn "Requesting video page for %s..." videoUrl
            let videoHtml = Http.RequestString(videoUrl, cookieContainer = cookies)
            let token = getSessionToken videoHtml
            getVideoId videoUrl |> getCommentUrl |> getCommentJson token videoUrl

        // request and parse the JSON comments
        let commentBodies =
            videoUrls
            |> Seq.map getVideoCommentJson
            |> Seq.map JsonValue.Parse
            |> Seq.map (fun j -> j.GetProperty("body").GetProperty("watch-discussion"))
        
        // parses the embedded HTML from a JSON comment result, returning the comment text
        let getCommentText (body: JsonValue) =
            let commentHtml = HtmlDocument.Parse(body.AsString())
            commentHtml.Descendants (fun n -> n.HasClass "comment-text-content")
            |> Seq.map HtmlNodeExtensions.InnerText

        commentBodies |> Seq.map getCommentText |> Seq.collect id