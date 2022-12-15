type Operations = | Noop | AddX of int
let getCycles = function | Noop -> 1 | AddX _ -> 2

let readFile filePath = System.IO.File.ReadLines filePath

let (|Prefix|_|) (prefix: string) (string: string) =
    if string.StartsWith prefix then Some (string.Substring prefix.Length)
    else None

let mapToOperations line =
    match line with
    | "noop" -> Some Noop
    | Prefix "addx " value -> Some (AddX (int value))
    | _ -> None

type State = { Cycle: int; RegisterX: int; NextRead: int; ReadInterval: int; Intervals: (int * int) list; RegisterXHistory: (int*int) list }
let handleOperations state input =
    let runCycles s c =
        match s, c, input with
        | _, _, AddX x when s.Cycle = s.NextRead && c-1 = 0 -> {s with Cycle=s.Cycle+1; RegisterX=s.RegisterX+x; NextRead=s.NextRead+s.ReadInterval; Intervals=(s.Cycle,s.Cycle*s.RegisterX)::s.Intervals; RegisterXHistory=(s.Cycle,s.RegisterX)::s.RegisterXHistory}
        | _, _, AddX x when c-1 = 0 -> {s with Cycle=s.Cycle+1;RegisterX=s.RegisterX+x; RegisterXHistory=(s.Cycle,s.RegisterX)::s.RegisterXHistory}
        | _ when s.Cycle = s.NextRead -> {s with Cycle=s.Cycle+1; NextRead=s.NextRead+s.ReadInterval; Intervals=(s.Cycle,s.Cycle*s.RegisterX)::s.Intervals; RegisterXHistory=(s.Cycle,s.RegisterX)::s.RegisterXHistory}
        | _ -> {s with Cycle=s.Cycle+1; RegisterXHistory=(s.Cycle,s.RegisterX)::s.RegisterXHistory}
    [1..input |> getCycles] |> List.rev |> List.fold runCycles state
    
type ScreenState = {Lines: string list; CurrentLine: string}
let getScreenOutput state (cycle,regX) =
    let position = (cycle-1)%40
    let currentLine = if position = 0 then "" else state.CurrentLine
    let getCharacter = match cycle,regX with |_ when position >= regX-1 && position <= regX+1 -> "#" | _ -> "."
    let newLine = currentLine + getCharacter
    let lines = if cycle%40 = 0 then newLine::state.Lines else state.Lines
    {Lines=lines; CurrentLine=newLine}

let defaultState = { Cycle=1; RegisterX=1; NextRead=20; ReadInterval=40; Intervals=[]; RegisterXHistory=[] }
let state = "../aoc2022-input/day10/input.txt" |> readFile |> Seq.map mapToOperations |> Seq.choose id |> Seq.fold handleOperations defaultState 
state |> (fun x -> x.Intervals) |> List.sumBy snd |> printfn "Part1=%i"

state |> (fun x -> x.RegisterXHistory) |> List.rev |> List.fold getScreenOutput {Lines=[]; CurrentLine=""} |> (fun x -> "Part2"::(List.rev x.Lines)) |> List.iter (printfn "%s")