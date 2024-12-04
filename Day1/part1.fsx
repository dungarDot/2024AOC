open System.IO
open System 

let inputData = File.ReadLines @".\input.txt"

let result = 
    inputData
    |> Seq.map(fun line ->
        let pairs = 
            line.Split "   "
            |> Array.map int
        (pairs[0],pairs[1])
    )
    |> Seq.toList
    |> List.unzip
    |> fun (l,r) ->
        let left, right = (l |> List.sort), (r |> List.sort)
        
        left
        |> List.mapi(fun i entry ->
            abs (entry - right[i])
        )
    |> List.sum

printfn "%A" result