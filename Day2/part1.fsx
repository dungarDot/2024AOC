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
    {   RemainingReport:Report
        PreviousLevel: Level
        Stability: Stability
        OriginalReport:Report } 

module ParsingReportState =
    let Create (report: UnCheckedReport) =
        {   RemainingReport = report.Tail
            PreviousLevel = report.Head
            Stability = StartingStability
            OriginalReport = report }

type CheckedReport = 
    | Safe of Report * Stability
    | Unsafe of Report * FailureReason

// I'm keeping the stability value on the assumption that if this were a real project you wouldn't want to just discard that data.

let unCheckedReports : UnCheckedReports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
        |> Array.toList
    )
    |> Seq.toList

let rec parseReport state = 
    let currentLevel, remainingReport = state.RemainingReport.Head, state.RemainingReport.Tail
    let levelDiff = abs (currentLevel - state.PreviousLevel)
    if levelDiff = 0 then 
        Unsafe (state.OriginalReport, NoChangeBetweenLevels)
    elif levelDiff > 3 then
        Unsafe (state.OriginalReport, JumpGreaterThan3)
    else 
        parseReport { state with RemainingReport = remainingReport }



