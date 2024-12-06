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
    | Unstable

type LevelVolatility =
    | WithinBounds
    | JumpGreaterThan3
    | NoChangeBetweenLevels

module LevelVolatility =
    // Make active pattern?
    let Verify current previous  =
        let diff = abs(current - previous)
        if diff = 0 then 
            NoChangeBetweenLevels
        elif diff > 3 then 
            JumpGreaterThan3
        else
            WithinBounds

type FailureReasons =
    | Unstable
    | Volatile of LevelVolatility
    | UnstableAndVolatile of LevelVolatility

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

// I'm keeping the stability value on the assumption that if this were a real project you wouldn't want to just discard that data.
type CheckedReport = 
    | Safe of Report * Stability
    | Unsafe of Report * FailureReasons

let unCheckedReports : UnCheckedReports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
        |> Array.toList
    )
    |> Seq.toList

let checkSafety current previous =
    let volatility = LevelVolatility.Verify current previous
    
    volatility
    
let rec parseReport state = 
    let currentLevel, remainingReport = state.RemainingReport.Head, state.RemainingReport.Tail
    let levelDiff = abs (currentLevel - state.PreviousLevel)
    if levelDiff = 0 then 
        parseReport { state with RemainingReport = remainingReport }
    elif levelDiff > 3 then
        parseReport { state with RemainingReport = remainingReport }
    else 
        parseReport { state with RemainingReport = remainingReport }
