#r "./packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#load "XPathParser.fs"

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
test xPathParts "div/ul/*[💩]"
