﻿module HelloSquare

let cube x = x * x * x

[<EntryPoint>]
let main argv =
    printfn "%d cubed is: %d!" 12 (cube 12)
    System.Console.ReadKey() |> ignore
    0 // Return an integer exit code