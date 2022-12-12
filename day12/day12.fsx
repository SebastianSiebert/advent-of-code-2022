open System.Diagnostics

type Direction = | Right | Left | Up | Down
type Coordinates = { X: int; Y: int; Elevation: int; Position: bool; Goal: bool; Visited: bool  }
let directions = [(-1,0);(1,0);(0,-1);(0,1)]

let readFile filePath = System.IO.File.ReadLines filePath

let rec mapLines index line =
    let mapValues i char =
        let charToInt c = int c - int 'a'
        let start = char = 'S'
        let goal = char = 'E'
        let elevation = match char with | 'S' -> charToInt 'a' | 'E' -> charToInt 'z' | _ -> charToInt char
        { X=index; Y=i; Elevation=elevation; Position=start; Goal=goal; Visited=start }
    line |> Seq.mapi mapValues
    
let tryFindPosition coords = Seq.tryFind (fun c -> c.Position) coords
let filterVisited coords = Seq.filter (fun c -> c.Visited) coords
        
let mapPosition cp (x,y) pos =
    if (pos = cp)
    then {pos with Position=false}
    else
        match pos with
        | {X=x';Y=y';Elevation=e;Visited=v} when x' = cp.X + x && y' = cp.Y + y && e >= cp.Elevation && e <= cp.Elevation + 1 && not v ->
            {pos with Position=true; Visited=true}
        | _ -> pos
        
let posFilter coords cp (x,y) =
    let positions = coords |> Seq.tryFind (fun e -> e.X = cp.X + x && e.Y = cp.Y + y && e.Elevation <= cp.Elevation + 1 && not e.Visited)
    match positions with | None -> false | Some _ -> true
        
let rec traverseGrid coords results direction =
    let currentPos = coords |> tryFindPosition
    match currentPos with
    | None -> results
    | Some cp when cp.Goal -> (coords |> filterVisited |> Seq.length)::results
    | Some cp ->
        let newPositions = directions |> List.filter (posFilter coords cp)
        match newPositions.Length with
        | length when length = 0 -> results
        | _ ->
            let newCoords = coords |> Seq.map (mapPosition cp direction)
            let visited = newCoords |> filterVisited |> Seq.length
            match results |> List.tryFind (fun e -> visited >= e) with
            | Some _ -> results
            | None -> directions |> List.map (traverseGrid newCoords results) |> List.concat
        

let coords = "../aoc2022-input/day12/input.txt" |> readFile |> Seq.mapi mapLines |> Seq.concat |> Seq.toList

let stopwatch = Stopwatch.StartNew()
directions |> List.map (traverseGrid coords []) |> List.concat |> List.min |> (fun visited -> visited - 1) |> printfn "Part1 = %i"
stopwatch.Stop()
printfn $"Elapsed: {stopwatch.Elapsed}"