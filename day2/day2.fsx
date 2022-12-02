open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let lineToValue (line: string) =
    match String.IsNullOrEmpty line with
    | true -> None
    | false ->
        let parts = line.Split " "
        Some (parts[0],parts[1])
        
let filterSome value =
    match value with
    | Some _ -> true
    | None -> false
    
let getValues value =
    match value with
    | Some value -> value
    | None -> ("","")

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
    
let mapElfToSelection elfSelection =
    match elfSelection with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> UndefinedSelection
    
let mapPlayerToSelection playerSelection =
    match playerSelection with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> UndefinedSelection
    
let mapDesiredOutcome playerOutcome =
    match playerOutcome with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> UndefinedOutcome

let getOutcome selections =
    match selections with
    | Rock, Rock -> Draw
    | Rock, Scissors -> Loss
    | Rock, Paper -> Win
    | Paper, Rock -> Loss
    | Paper, Scissors -> Win
    | Paper, Paper -> Draw
    | Scissors, Rock -> Win
    | Scissors, Scissors -> Draw
    | Scissors, Paper -> Loss
    | _ -> UndefinedOutcome
    
let getPlayerSelection selections =
    match selections with
    | Rock, Win -> Paper
    | Rock, Draw -> Rock
    | Rock, Loss -> Scissors
    | Paper, Win -> Scissors
    | Paper, Draw -> Paper
    | Paper, Loss -> Rock
    | Scissors, Win -> Rock
    | Scissors, Draw -> Scissors
    | Scissors, Loss -> Paper
    | _ -> UndefinedSelection
    
let mapSelections selections =
    let elf, player = selections
    let elfSelection = mapElfToSelection elf
    let playerSelection = mapPlayerToSelection player
    (elfSelection, playerSelection)
    
let calculatePoints selections =
    let _, player = selections
    let playerSelectionScore = getSelectionPoint player
    let outcomeScore = selections |> getOutcome |> getOutcomePoint
    let score = playerSelectionScore + outcomeScore
    score

let mapSelectionsPart2 selection =
    let elf, outcome = selection
    let elfSelection = mapElfToSelection elf
    let outcomeValue = mapDesiredOutcome outcome
    let playerSelection = getPlayerSelection (elfSelection, outcomeValue)
    (playerSelection, outcomeValue)
    
let mapDesiredOutcomeToPoints desiredOutcome =
    let player, outcome = desiredOutcome
    let playerSelectionScore = getSelectionPoint player
    let outcomeScore = getOutcomePoint outcome
    let score = playerSelectionScore + outcomeScore
    score
    
let getValuesFromInput = readLines >> Seq.map lineToValue >> Seq.filter filterSome >> Seq.map getValues

let scorePart1 = "input.txt" |> getValuesFromInput |> Seq.map mapSelections |> Seq.map calculatePoints |> Seq.sum
printfn $"Part1 Points = %i{scorePart1}"

let scorePart2 = "input.txt" |> getValuesFromInput |> Seq.map mapSelectionsPart2 |> Seq.map mapDesiredOutcomeToPoints |> Seq.sum
printfn $"Part2 Points = %A{scorePart2}"
