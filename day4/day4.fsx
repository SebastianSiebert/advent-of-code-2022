type ElfDuties = { StartElf1 : int; EndElf1 : int; StartElf2 : int; EndElf2: int }

let readFile filePath = System.IO.File.ReadAllText filePath

let splitInput (input: string) = input.Split "\n"

let prepareInput (input: string) =
    let parts = input.Split ","
    let sections1 = parts[0].Split "-"
    let sections2 = parts[1].Split "-"
    { StartElf1 = int sections1[0]; EndElf1 = int sections1[1]; StartElf2 = int sections2[0]; EndElf2 = int sections2[1] }
    
let findCompleteOverlap elfDuty =
    match elfDuty with
    | {StartElf1 = se1; EndElf1 = ee1; StartElf2 = se2; EndElf2 = ee2} when se1 >= se2 && ee1 <= ee2 -> true
    | {StartElf1 = se1; EndElf1 = ee1; StartElf2 = se2; EndElf2 = ee2} when se2 >= se1 && ee2 <= ee1 -> true
    | _ -> false
    
let findOverlap elfDuty =
    match elfDuty with
    | {StartElf1 = se1; EndElf1 = ee1; StartElf2 = se2; EndElf2 = ee2} when (se1 >= se2 && se1 <= ee2) || (ee1 >= se2 && ee1 <= ee2) -> true
    | {StartElf1 = se1; EndElf1 = ee1; StartElf2 = se2; EndElf2 = ee2} when (se2 >= se1 && se2 <= ee1) || (ee2 >= se1 && ee2 <= ee1) -> true
    | _ -> false

"input.txt" |> readFile |> splitInput |> Array.map prepareInput |> Array.filter findCompleteOverlap |> Array.length |> printfn "Part1 = %i"
"input.txt" |> readFile |> splitInput |> Array.map prepareInput |> Array.filter findOverlap |> Array.length |> printfn "Part2 = %i"