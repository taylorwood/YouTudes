namespace YouTudes

module Utility =

    // http://fsharpforfunandprofit.com/posts/computation-expressions-intro/
    type MaybeBuilder() =

        member __.Bind(x, f) = 
            match x with
            | None -> None
            | Some a -> f a

        member __.Return(x) = 
            Some x
   
    let maybe = new MaybeBuilder()
