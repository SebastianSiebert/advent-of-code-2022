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

type State = { Cycle: int; RegisterX: int; NextRead: int; ReadInterval: int; Intervals: (int * int) list }
let handleOperations state input =
    let runCycles s c =
        match s, c, input with
        | _, _, AddX x when s.Cycle = s.NextRead && c-1 = 0 -> {s with Cycle=s.Cycle+1; RegisterX=s.RegisterX+x; NextRead=s.NextRead+s.ReadInterval; Intervals=(s.Cycle,s.Cycle*s.RegisterX)::s.Intervals}
        | _, _, AddX x when c-1 = 0 -> {s with Cycle=s.Cycle+1;RegisterX=s.RegisterX+x}
        | _ when s.Cycle = s.NextRead -> {s with Cycle=s.Cycle+1; NextRead=s.NextRead+s.ReadInterval; Intervals=(s.Cycle,s.Cycle*s.RegisterX)::s.Intervals}
        | _ -> {s with Cycle=s.Cycle+1}
    [1..input |> getCycles] |> List.rev |> List.fold runCycles state

let defaultState = { Cycle=1; RegisterX=1; NextRead=20; ReadInterval=40; Intervals=[] }
"../aoc2022-input/day10/input.txt" |> readFile |> Seq.map mapToOperations |> Seq.choose id |> Seq.fold handleOperations defaultState |> (fun x -> x.Intervals) |> List.sumBy snd |> printfn "Part1=%i"