open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let lineToValue (line: string) =
    match String.IsNullOrEmpty line with
    | true -> None
    | false -> Some (line |> int)
    
let calculateCaloriesPerELf lines =
    let initialValue = [0]
    let action caloriesPerElf calories =
        let calculateNewCalories value =
            match caloriesPerElf with
            | first::rest ->
                let newValue = first + value
                newValue::rest
            | _ -> caloriesPerElf
        match calories with
        | Some cal -> calculateNewCalories cal
        | None -> 0::caloriesPerElf
    lines |> Seq.fold action initialValue
    
let calculateSumOfTopThreeCalories =  List.sortDescending >> List.take 3 >> List.sum

let caloriesPerElf = "input.txt" |> readLines |> Seq.map lineToValue |> calculateCaloriesPerELf
caloriesPerElf |> List.max |> printfn "Max Calories = %i"
caloriesPerElf |> calculateSumOfTopThreeCalories |> printfn "Top three Calories = %i"