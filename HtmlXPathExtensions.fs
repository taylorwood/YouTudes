namespace YouTudes

open System
open FSharp.Data
open System.Runtime.CompilerServices
open XPathParser

module HtmlNodeXPath =

    let equals a b = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let compareValues (n: HtmlNode) attr a b =
        if equals attr "class" then n.HasClass(b)
        else equals a b

    let satisfiesPredicate (n: HtmlNode) pred =
        let attr = n.TryGetAttribute(pred.Attribute)
        match attr with
        | Some a ->
            match pred.Filter with
            | Some (op, comp) -> // perform comparison
                let value = a.Value()
                match op with
                | Equals -> compareValues n pred.Attribute value comp
                | NotEquals -> not <| compareValues n pred.Attribute value comp
            | None -> true // attr found, no comparison
        | None -> false // attr not found

    let evaluate' part (n: HtmlNode) =
        let axis, name, preds = part
        let searchNodes =
            match axis with
            | Child ->
                if name <> "*" then n.Elements(name) else n.Elements()
                |> Seq.ofList
            | DescendantOrSelf ->
                if name <> "*" then n.DescendantsAndSelf(name) else n.DescendantsAndSelf()
        let isMatch n = preds |> List.forall (satisfiesPredicate n)
        searchNodes |> Seq.where isMatch

    let evaluate xPath node =
        let rec loop parts nodes =
            match parts with
            | [] -> nodes
            | p::ps ->
                let ns = nodes |> Seq.map (evaluate' p) |> Seq.collect id |> List.ofSeq
                loop ps ns
        loop xPath [node]

[<Extension>]
type HtmlXPathExtensions =

    [<Extension>]
    static member Select(n: HtmlNode, xPath: string) =
        let result = XPathParser.parse xPath
        match result with
        | Choice1Of2 parts -> HtmlNodeXPath.evaluate parts n
        | Choice2Of2 error -> failwith error

    [<Extension>]
    static member Select(doc: HtmlDocument, xPath: string) =
        let result = XPathParser.parse xPath
        match result with
        | Choice1Of2 parts ->
            let elems = doc.Elements() // evals XPath each top-level element
            seq { for elem in elems do
                    yield! HtmlNodeXPath.evaluate parts elem }
        | Choice2Of2 error -> failwith error
