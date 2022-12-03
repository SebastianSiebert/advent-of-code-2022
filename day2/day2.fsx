open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let checkLineNotEmpty line = line |> String.IsNullOrWhiteSpace |> not

let lineToValue (line: string) =
    let parts = line.Split " "
    (parts[0],parts[1])

type Selection = Rock | Paper | Scissors | UndefinedSelection
type Outcome = Win | Loss | Draw | UndefinedOutcome

let getSelectionPoint selection =
    match selection with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
    | UndefinedSelection -> 0

let getOutcomePoint outcome =
    match outcome with
    | Loss -> 0
    | Draw -> 3
    | Win -> 6
    | UndefinedOutcome -> 0

let mapToSelection elfSelection =
    match elfSelection with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> UndefinedSelection

let mapDesiredOutcome playerOutcome =
    match playerOutcome with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> UndefinedOutcome

let getOutcome selections =
    match selections with
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Loss
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
    | _ -> UndefinedOutcome

let getPlayerSelection selections =
    match selections with
    | Rock, Win | Paper, Draw | Scissors, Loss -> Paper
    | Rock, Draw | Paper, Loss | Scissors, Win -> Rock
    | Rock, Loss | Paper, Win | Scissors, Draw -> Scissors
    | _ -> UndefinedSelection
    
let mapSelections (elf, player) =
    let elfSelection = mapToSelection elf
    let playerSelection = mapToSelection player
    let outcomeValue = (elfSelection, playerSelection) |> getOutcome
    (playerSelection, outcomeValue)

let mapSelectionsPart2 (elf, outcome) =
    let elfSelection = mapToSelection elf
    let outcomeValue = mapDesiredOutcome outcome
    let playerSelection = getPlayerSelection (elfSelection, outcomeValue)
    (playerSelection, outcomeValue)
    
let calculatePoints (player, outcome) =
    let playerSelectionScore = getSelectionPoint player
    let outcomeScore = getOutcomePoint outcome
    let score = playerSelectionScore + outcomeScore
    score
    
let calculateScore mapSelections = readLines >> Seq.filter checkLineNotEmpty >> Seq.map lineToValue >> Seq.map mapSelections >> Seq.map calculatePoints >> Seq.sum

let filePath = "input.txt"
filePath |> calculateScore mapSelections |> printfn "Part1 Points = %i"
filePath |> calculateScore mapSelectionsPart2 |> printfn "Part2 Points = %i"
