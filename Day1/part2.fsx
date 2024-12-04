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
    |> fun (left, right) -> 
        left
        |> List.map( fun entry ->
            let numOfEntries =
                (right |> List.filter(fun value -> value = entry) ).Length
            entry * numOfEntries
        )
    |> List.sum
    
printfn "%A" result