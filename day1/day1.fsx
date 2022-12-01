open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let lineToValue (line: string) =
    match String.IsNullOrEmpty line with
    | true -> None
    | false -> Some (line |> int)
    
let getCaloriesPerELf lines =
    let initialValue = [0]
    let action caloriesPerElf x =
        let calories = lineToValue x
        match calories with
        | Some cal ->
            match caloriesPerElf with
            | first::rest ->
                let newValue = first + cal
                newValue::rest
            | _ -> caloriesPerElf
        | None -> 0::caloriesPerElf
    lines |> Seq.fold action initialValue
    
let calculateMaxCalories = readLines >> getCaloriesPerELf >> List.max
let calculateTopThreeCalories = readLines >> getCaloriesPerELf >> List.sortDescending >> List.take 3 >> List.sum

let filePath = @"input.txt"
let maxCalories = calculateMaxCalories filePath
printfn $"Max Calories = %i{maxCalories}"
let topThreeCalories = calculateTopThreeCalories filePath
printfn $"Top three Calories = %i{topThreeCalories}"