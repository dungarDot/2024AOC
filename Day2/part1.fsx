open System.IO

// Shooting for a more DDD approach.  We'll see how much I stick with it.

type Level = int 
type Report  = Level list 
type UnCheckedReport  = Report
type UnCheckedReports = UnCheckedReport list

type Stability =
    | StartingStability
    | Increasing
    | Decreasing

type FailureReason =
    | Unstable
    | JumpGreaterThan3 
    | NoChangeBetweenLevels

type ParsingReportState = 
    | Starting
    | StillSafe of Level * Stability
    | Failed of FailureReason

type CheckedReport = 
    | Safe of Report * Stability
    | Unsafe of FailureReason

// I'm keeping the stability value on the assumption that if this were a real project you wouldn't want to just discard that data.

let unCheckedReports : UnCheckedReports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
        |> Array.toList
    )
    |> Seq.toList

let rec parseReport (report:Report) state = 
    match state with 
    | Starting -> 
        parseReport report.Tail (StillSafe (report.Head, StartingStability))  
    | StillSafe (previous, stab) -> ()
    | Failed failReason -> ()

