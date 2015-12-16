open System
open System.IO
open YouTudes

[<EntryPoint>]
let main argv =

    Console.Write "Please enter a search query: "
    let query = Console.ReadLine()

    let comments = YouTubeComments.getComments query

    let fileName =
        match Seq.tryHead argv with
        | Some f -> f
        | None -> // no file name specified, use query
            let invalidChars = Path.GetInvalidFileNameChars()
            let withoutInvalidChars = Seq.where (fun c -> invalidChars |> Seq.contains c |> not)
            let fileName = String(query |> withoutInvalidChars |> Array.ofSeq)
            Path.Combine(Directory.GetCurrentDirectory(), fileName + ".txt")
    
    printfn "Appending comments to %s" fileName
    File.AppendAllLines(fileName, comments)

    0 // return an integer exit code
