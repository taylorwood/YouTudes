namespace YouTudes

open System
open FParsec

module XPathParser =

    type XPathPart = XPathAxis * NodeTest * Predicate list
    and XPathAxis =
        | Child
        | DescendantOrSelf
    and NodeTest = string
    and Operator =
        | Equals
        | NotEquals
    and Predicate = {
        Attribute: string
        Filter: (Operator * string) option
    }

    type XPathParser = Parser<XPathAxis, unit>

    let separator : XPathParser = stringReturn "/" Child
    let descOrSelf = stringReturn "//" DescendantOrSelf
    let xPathAxis = choice [descOrSelf; separator; preturn Child]

    let tagName = many1Satisfy (fun c -> isAsciiLetter c || isDigit c)
    let nodeName = choice [tagName; pstring "*"]

    let predicateName = many1Satisfy (fun c -> isAsciiLetter c || isDigit c || c = '-')

    let eqOp = stringReturn "=" Equals
    let uneqOp = stringReturn "!=" NotEquals
    let operand = spaces >>. choice [eqOp; uneqOp] .>> spaces
    let quoted = between (pstring "'") (pstring "'") predicateName
    let comparison = operand .>>. choice [quoted; predicateName]

    let predicate = between (pstring "[@") (pstring "]") (predicateName .>>. opt comparison)

    let getXPathPart axis name predicates =
        let predicates =
            predicates |> List.map (fun (attr, comp) -> { Attribute = attr; Filter = comp })
        axis, name, predicates

    let xPathPart =
        let predicates = many predicate
        pipe3 xPathAxis nodeName predicates getXPathPart

    let xPathParts = (many1 xPathPart) .>> eof

    let parse str =
        let result = run xPathParts str
        match result with
        | Success (result, _, _) -> Choice1Of2 result
        | Failure (msg, _, _) -> Choice2Of2 <| sprintf "Failure: %s" msg
