open impl

[<EntryPoint>]
let main argv = 
    addNumbers 10 20
    |> printfn "addNumbers 10 20 = %d" 

    replicate 1 5
    |> printfn "replicate 1 5 = %A"

    fibonacci 10
    |> printfn "fibonacci 10 = %A"

    System.Console.ReadKey () 
    |> ignore

    0
