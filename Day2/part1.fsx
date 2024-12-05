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
    | Starting of Report
    | StillSafe of 
        {|  RemainingReport:Report
            PreviousLevel: Level
            Stability: Stability
            OriginalReport:Report|} 
    | Failed of Report * FailureReason

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
    match state with 
    | Starting startingReport -> 
        let initState = 
            StillSafe {|
                RemainingReport = startingReport.Tail
                PreviousLevel = startingReport.Head
                Stability = StartingStability
                OriginalReport = startingReport |}

        parseReport initState

    | StillSafe currentState -> 
        match currentState.RemainingReport with 
        | currentLevel::remainingReport -> 
            let levelDiff = abs (currentLevel - currentState.PreviousLevel)
            if levelDiff = 0 then 
                Unsafe (currentState.OriginalReport, NoChangeBetweenLevels)
            else 
                let newState = StillSafe {| currentState with RemainingReport = remainingReport |}
                parseReport newState

        | [] -> Safe (currentState.OriginalReport, currentState.Stability)
    | _ -> 
        let message = sprintf "impossible match of %A" state
        failwith message

