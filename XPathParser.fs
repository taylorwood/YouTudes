namespace YouTudes

open System
open FParsec

module XPathParser =

    type XPathPart = Axis * NodeTest * Predicate list
    and Axis =
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

    let separator : Parser<Axis, unit> = stringReturn "/" Child
    let descOrSelf = stringReturn "//" DescendantOrSelf
    let axis = choice [descOrSelf; separator; preturn Child]

    let tagName = many1Satisfy (fun c -> isAsciiLetter c || isDigit c)
    let nodeName = choice [tagName; pstring "*"]

    let equal = stringReturn "=" Equals
    let unequal = stringReturn "!=" NotEquals
    let operator = spaces >>. choice [equal; unequal] .>> spaces
    let predicateName = many1Satisfy (fun c -> isAsciiLetter c || isDigit c || c = '-')
    let quoted = between (pstring "'") (pstring "'")
    let comparison = operator .>>. choice [quoted predicateName; predicateName]
    let predicate = between (pstring "[@") (pstring "]") (predicateName .>>. opt comparison)

    let getXPathPart axis name predicates =
        let predicates =
            predicates |> List.map (fun (attr, comp) -> { Attribute = attr; Filter = comp })
        axis, name, predicates

    let xPathPart =
        let predicates = many predicate
        pipe3 axis nodeName predicates getXPathPart

    let xPathParts = (many1 xPathPart) .>> eof

    let parse str =
        let result = run xPathParts str
        match result with
        | Success (result, _, _) -> Choice1Of2 result
        | Failure (msg, _, _) -> Choice2Of2 <| sprintf "Failure: %s" msg
