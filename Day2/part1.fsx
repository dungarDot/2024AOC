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

    let (|Check|) (current, previous, stability) =
        let stabLogic current previous greaterOutcome lesserOutcome sameOutcome =
            if current > previous then 
                greaterOutcome
            elif current < previous then 
                lesserOutcome
            else 
                sameOutcome

        match stability with 
        | StartingStability -> 
            // This currently exists but probably should be removed as the data isn't saved anyways and can't output, but the this branch does occur.
            // Ideal output probably does carry this all the way through so you can see the "state" of the report when you're done and why it failed.
            // Thus need to refactor the safe/unsafe returns to likely include state.
            stabLogic current previous Increasing Decreasing NoMovement  
        | Increasing -> stabLogic current previous Increasing Unstable Increasing
        | Decreasing -> stabLogic current previous Unstable Decreasing Decreasing
        | Unstable
        | NoMovement -> failwith "matched Unstable/NoMovement on the Stability verify."

    let Verify current previous stability : Stability =
        match (current, previous, stability) with
        | Check result -> result

// Allows invalid states grrrr.
type LevelVolatility =
    | WithinBounds
    | JumpGreaterThan3
    | NoChangeBetweenLevels

module LevelVolatility =
    let (|Check|) (current:Level, previous:Level) : LevelVolatility =
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
    | UnstableAndJumpGreaterThan3

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

module CheckedReport = 
    let ToString result = 
        match result with 
        | Safe (r,s) -> 
            let report =
                r |> List.map string |> String.concat ", " 
            sprintf "%s | %s" report (s.ToString())
        | Unsafe (r, f ) ->
            let report =
                r |> List.map string |> String.concat ", "
            sprintf "%s | %s" report (f.ToString()) 

let unCheckedReports : UnCheckedReports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
        |> Array.toList
    )
    |> Seq.toList

let checkSafety  stability volatility state (currentLevel:Level) (previousLevel:Level) f =

    match stability, volatility with 
    // Error states, ideally should refactor so these aren't even possible
    | StartingStability, _ -> failwith "matched starting stability on the parseReport function which should not be possible"
    | NoMovement, WithinBounds -> failwith "matched NoMovement, WithinBounds which should not be possible"
    // Unsafe states
    | Unstable , JumpGreaterThan3
    | Unstable, NoChangeBetweenLevels  -> Unsafe(state.OriginalReport, UnstableAndJumpGreaterThan3)
    | Unstable, WithinBounds -> Unsafe(state.OriginalReport, UnsafeStability)
    | _, JumpGreaterThan3 -> Unsafe(state.OriginalReport, Volatile volatility)
    | _, NoChangeBetweenLevels -> Unsafe(state.OriginalReport, Volatile volatility)
    // Safe states
    | Increasing, WithinBounds
    | Decreasing, WithinBounds -> 
        if state.RemainingReport.Length = 1 then 
            // No more checks required, return report
            Safe (state.OriginalReport, state.Stability)
        else 
            // remaining checks
            let newState = 
                { state with 
                    Stability = stability
                    PreviousLevel = currentLevel
                    RemainingReport = state.RemainingReport.Tail }
            f newState

let rec parseReport state = 
    let currentLevel = state.RemainingReport.Head
    let stability = Stability.Verify currentLevel state.PreviousLevel state.Stability
    let volatility = 
        match (currentLevel, state.PreviousLevel) with 
        | LevelVolatility.Check lv -> lv

    checkSafety stability volatility state currentLevel state.PreviousLevel parseReport

let output =
    unCheckedReports
    |> List.map (
            ParsingReportState.Create 
            >> parseReport
            >> CheckedReport.ToString)

File.WriteAllLines("./Day2/output.txt", output)

