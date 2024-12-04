open System.IO

// Shooting for a more DDD approach.  We'll see how much I stick with it.

type Level = int 
type Report  = Level array 
type Reports = Report list

type Stability =
    | StartingStability
    | Increasing
    | Decreasing

type Reason =
    | Unstable
    | JumpGreaterThan3 
    | NoChange

type ReportSafety = 
    | Unchecked
    | Safe
    | Unsafe of Reason

type ReportDetails =
    {   Report:Report
        Safety: ReportSafety
        Stability: Stability }

module ReportDetails = 
    let Create report =
        {   Report = report 
            Safety = Unchecked 
            Stability = StartingStability }
            
let reports : Reports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
    )
    |> Seq.toList

let ParseReport report = 
    let startingState = ReportDetails.Create report
