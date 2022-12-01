open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let lineToValue (line: string) =
    match String.IsNullOrEmpty line with
    | true -> None
    | false -> Some (line |> int)
    
let getCaloriesPerELf lines =
    let initialValue = [0]
    let action elfsSoFar x =
        let calories = lineToValue x
        match calories with
        | Some cal ->
            match elfsSoFar with
            | [element] ->
                let newValue = element + cal
                [newValue]
            | first::rest ->
                let newValue = first + cal
                newValue::rest
        | None -> 0::elfsSoFar
    lines |> Seq.fold action initialValue |> List.rev
    
let getMaxCalories caloriesPerElf = caloriesPerElf |> List.max

let sumFirstThree list =
    match list with
    | [] -> None
    | _ ->
        let initialValue = (0,0)
        let action state x =
            let (number, value) = state
            match number with
            | number when number < 3 ->
                let newNumber = number + 1
                let newValue = value + x
                (newNumber,newValue)
            | _ -> state
        Some (list |> List.fold action initialValue)

let calculateMaxCalories = readLines >> getCaloriesPerELf >> getMaxCalories
let calculateTopThreeCalories =
    readLines >>
    getCaloriesPerELf >>
    List.sort >>
    List.rev >>
    sumFirstThree

let filePath = @"day1\input.txt"
let maxCalories = calculateMaxCalories filePath
printfn $"Max Calories = %i{maxCalories}"
let topThreeCalories = calculateTopThreeCalories filePath
match topThreeCalories with
| None -> printfn "No Calories found"
| Some (_,calories) -> printfn $"Top three Calories = %i{calories}"