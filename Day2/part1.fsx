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

let rec parseReport state = 
    let currentState = {state with RemainingReport = state.RemainingReport.Tail}
    let currentLevel= state.RemainingReport.Head
    let stability = Stability.Verify currentLevel state.PreviousLevel state.Stability
    let volatility = LevelVolatility.Verify currentLevel state.PreviousLevel

    match stability, volatility with 
    // Error states, ideally should refactor so these aren't even possible
    | StartingStability, _ -> failwith "matched starting stability on the parseReport function which should not be possible"
    | NoMovement, WithinBounds -> failwith "matched NoMovement, WithinBounds which should not be possible"
    // Unsafe states
    | Unstable , JumpGreaterThan3
    | Unstable, NoChangeBetweenLevels  -> Unsafe(state.OriginalReport, UnstableAndVolatile volatility)
    | Unstable, WithinBounds -> Unsafe(state.OriginalReport, UnsafeStability)
    | _, JumpGreaterThan3 -> Unsafe(state.OriginalReport, Volatile volatility)
    | _, NoChangeBetweenLevels -> Unsafe(state.OriginalReport, Volatile volatility)
    // Safe states
    | Increasing, WithinBounds
    | Decreasing, WithinBounds -> 
        if currentState.RemainingReport.Length = 0 then 
            Safe (currentState.OriginalReport, currentState.Stability)
        else 
            parseReport currentState