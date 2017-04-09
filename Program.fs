module CodeJam

open System
open ProblemC

/// Reads from STDIN until the input is null.
let readInput =
    fun _ -> Console.ReadLine()
    |>  Seq.initInfinite
    |>  Seq.takeWhile ((<>) null)

[<EntryPoint>]
let main argv =
    readInput 
    |> resolve
    |> Seq.iter (printfn "%s")
    0 // return an integer exit code
