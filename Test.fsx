#r "./packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#r "./packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#load "XPathParser.fs"
#load "HtmlXPathExtensions.fs"

open FSharp.Data
open FParsec
open YouTudes
open YouTudes.XPathParser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
 
test xPathParts "A/B/C/*"
test xPathParts "//div[@class='hide'][@disabled]//a[@href]"
test xPathParts "//footer[@class=some-class]//h2"
