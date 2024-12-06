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
        | Increasing -> stabLogic current previous Increasing Unstable Increasing
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
        let previousLevel  = report.Head
        {   RemainingReport = report.Tail
            PreviousLevel = previousLevel
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

let checkSafety  stability volatility state currentLevel f =
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
        if state.RemainingReport.Length = 1 then 
            Safe (state.OriginalReport, state.Stability)
        else 
            let newState = 
                { state with 
                    Stability = stability
                    PreviousLevel = currentLevel
                    RemainingReport = state.RemainingReport.Tail }
            f newState

let rec parseReport state = 
    let currentLevel = state.RemainingReport.Head
    printfn "%A" (currentLevel, state)
    
    let stability = Stability.Verify currentLevel state.PreviousLevel state.Stability
    let volatility = LevelVolatility.Verify currentLevel state.PreviousLevel
    checkSafety stability volatility state currentLevel parseReport

unCheckedReports
|> List.map (ParsingReportState.Create >> parseReport)
|> List.filter(fun result ->
    match result with 
    | Safe _ -> false
    | Unsafe (a, b) -> 
        match b with
        | UnstableAndVolatile _ -> true 
        | _ -> false
)
