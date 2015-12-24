namespace YouTudes

open System
open FSharp.Data
open System.Runtime.CompilerServices
open XPathParser

module HtmlNodeXPath =

    let compareValues (n: HtmlNode) attr a b =
        let (=~) a b = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)
        if attr =~ "class" then n.HasClass(b)
        else a =~ b

    let satisfiesPredicate (n: HtmlNode) pred =
        match n.TryGetAttribute(pred.Attribute) with
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
            | DescendantOrSelf ->
                if name <> "*" then n.DescendantsAndSelf(name) else n.DescendantsAndSelf()
                |> List.ofSeq
        let isMatch n = preds |> List.forall (satisfiesPredicate n)
        searchNodes |> List.where isMatch

    let evaluate xPath node =
        let folder nodes part = nodes |> List.collect (evaluate' part)
        xPath |> Seq.fold folder [node]

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
            let elems = doc.Elements() // evals XPath for each top-level element
            seq { for elem in elems do
                    yield! HtmlNodeXPath.evaluate parts elem }
        | Choice2Of2 error -> failwith error
