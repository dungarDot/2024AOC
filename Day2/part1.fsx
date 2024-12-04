open System.IO

// Shooting for a more DDD approach.  We'll see how much I stick with it.

type Level = int 
type Report  = Level array 
type Reports = Report list
type EscalationState =
    | Starting
    | Increasing
    | Decreasing
    | Invalid

type JumpState = 
    | Valid
    | InvalidNoDifference
    | InvalidJumpGreaterThan3

type ReportDetails =
    {   Report:Report
        EscalationState: EscalationState
        JumpState: JumpState }

type ReportResult = 
    | Safe of ReportDetails
    | Unsafe of ReportDetails

let reports : Reports = 
    File.ReadAllLines "./Day2/input.txt"
    |> Seq.map(fun line ->
        line.Split(" ")
        |> Array.map int 
    )
    |> Seq.toList