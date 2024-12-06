open System.IO

// Shooting for a more DDD approach.  We'll see how much I stick with it.
// Not going to handle checking that report length is > 0/x as i'd do that before all this.

type Level = int 
type Report  = Level list 
type UnCheckedReport  = Report
type UnCheckedReports = UnCheckedReport list

type Stability =
    | StartingStability
    | Increasing
    | Decreasing
    | Unstable
    | NoMovement // for tracking the error state of instantly passing two values that are the same.

module Stability = 
    // make active pattern?
    let private stabLogic current previous greaterOutcome lesserOutcome sameOutcome =
        if current > previous then 
            greaterOutcome
        elif current < previous then 
            lesserOutcome
        else 
            sameOutcome

    let Verify current previous stability =
        match stability with 
        | StartingStability -> stabLogic current previous Increasing Decreasing NoMovement
        | Increasing -> stabLogic current previous Increasing Decreasing Increasing
        | Decreasing -> stabLogic current previous Unstable Decreasing Decreasing
        | Unstable
        | NoMovement -> failwith "matched Unstable/NoMovement on the Stability verify."

// Allows invalid states grrrr.
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
    | UnsafeStability
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

let errorCheckStartingStability = 
    [
        [9; 10; 11; 12; 11; 8]
        [41; 38; 39; 40; 43; 42; 44; 44]
        [44; 43; 43; 45; 46; 46]
        [86; 84; 85; 85; 89]
        [59; 60; 57; 58; 56; 56]
        [61; 64; 62; 59; 62; 58]
        [76; 77; 74; 74; 77]
        [8; 10; 10; 9; 6; 6]
        [62; 59; 60; 61; 64; 64]
        [11; 10; 10; 13; 13]
        [31; 29; 33; 34; 34]
        [69; 71; 68; 67; 68; 70]
        [32; 34; 33; 35; 31]
        [14; 15; 15; 13; 12]
        [10; 7; 8; 8; 7]
        [56; 57; 54; 55; 55]
        [94; 95; 93; 91; 93]
    ]
let rec parseReport state = 
    let currentState = {state with RemainingReport = state.RemainingReport.Tail}
    let currentLevel= state.RemainingReport.Head

    let stability = Stability.Verify currentLevel currentState.PreviousLevel currentState.Stability
    let volatility = LevelVolatility.Verify currentLevel currentState.PreviousLevel

    match stability, volatility with 
    // Error states, ideally should refactor so these aren't even possible
    | StartingStability, _ -> failwith "matched starting stability on the parseReport function which should not be possible"
    | NoMovement, WithinBounds -> failwith "matched NoMovement, WithinBounds which should not be possible"
    // Unsafe states
    | Unstable , JumpGreaterThan3
    | Unstable, NoChangeBetweenLevels  -> Unsafe(currentState.OriginalReport, UnstableAndVolatile volatility)
    | Unstable, WithinBounds -> Unsafe(currentState.OriginalReport, UnsafeStability)
    | _, JumpGreaterThan3 -> Unsafe(currentState.OriginalReport, Volatile volatility)
    | _, NoChangeBetweenLevels -> Unsafe(currentState.OriginalReport, Volatile volatility)
    // Safe states
    | Increasing, WithinBounds
    | Decreasing, WithinBounds -> 
        if currentState.RemainingReport.Length = 0 then 
            Safe (currentState.OriginalReport, currentState.Stability)
        else 
            parseReport { currentState with Stability = stability }

unCheckedReports
|> List.map ParsingReportState.Create
|> List.map parseReport
|> List.filter(fun result -> 
    match result with 
    | Safe (_,s) -> 
        if s = StartingStability then true else false
    | _ -> false
)