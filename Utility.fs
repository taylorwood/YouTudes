namespace YouTudes

module Utility =

    // http://fsharpforfunandprofit.com/posts/computation-expressions-intro/
    type MaybeBuilder() =

        member this.Bind(x, f) = 
            match x with
            | None -> None
            | Some a -> f a

        member this.Return(x) = 
            Some x
   
    let maybe = new MaybeBuilder()
