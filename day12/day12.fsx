open System.Diagnostics

let startTime = Stopwatch.GetTimestamp()

let addTuple (ax,ay) (bx,by) = (ax+bx,ay+by)

type Coordinates = { Coordinates: int * int; Elevation: int; Position: bool; Goal: bool; Visited: bool  }
type Edge = { From: int*int; To: int*int; Cost: int }
let directions = [(-1,0);(1,0);(0,-1);(0,1)]

let readFile filePath = System.IO.File.ReadLines filePath

let rec mapLines index line =
    let mapValues i char =
        let charToInt c = int c - int 'a'
        let start = char = 'S'
        let goal = char = 'E'
        let elevation = match char with | 'S' -> charToInt 'a' | 'E' -> charToInt 'z' | _ -> charToInt char
        { Coordinates=(index,i); Elevation=elevation; Position=start; Goal=goal; Visited=start }
    line |> Seq.mapi mapValues
    
let tryFindPosition coords = Seq.tryFind (fun c -> c.Position) coords
let filterVisited coords = Seq.filter (fun c -> c.Visited) coords
        
let getEdges coords state currentPos =
    let possibleLocations = directions |> List.map (addTuple currentPos.Coordinates)
    let newPositions = coords |> List.filter (fun c -> List.contains c.Coordinates possibleLocations) |> List.filter (fun c -> c.Elevation <= currentPos.Elevation + 1) |> List.map (fun c -> {From=currentPos.Coordinates; To=c.Coordinates; Cost=1})
    List.append newPositions state
    
let findShortestPath startPos allEdges  =
    let rec addPaths edgesFromStart isDone =
        if isDone then edgesFromStart
        else
            let newEdges =
                allEdges
                |> List.filter (fun e -> edgesFromStart |> List.tryFind (fun x -> e.From = x.To) |> function | Some _ -> true | _ -> false)
                |> List.map (fun e ->
                    let edge = List.find (fun x -> x.To = e.From) edgesFromStart
                    {From=edge.From; To=e.To; Cost=edge.Cost + e.Cost})
                |> List.append edgesFromStart
                |> List.groupBy (fun e -> e.To)
                |> List.map (fun (_,e) -> e |> List.minBy (fun x -> x.Cost))
                |> List.sortBy (fun e -> (e.From, e.To, e.Cost))
            addPaths newEdges (edgesFromStart = newEdges)
    let edges = allEdges |> List.filter (fun e -> List.exists (fun x -> e.From = x.Coordinates) startPos) |> List.sortBy (fun e -> (e.From, e.To, e.Cost))
    // let edges = allEdges |> List.filter (fun e -> e.From = startPos.Coordinates) |> List.sortBy (fun e -> (e.From, e.To, e.Cost))
    let allEdgesFromStart = addPaths edges false
    
    (fun endPos ->
        allEdgesFromStart |> List.tryFind (fun e -> e.To = endPos))

let coords = "../aoc2022-input/day12/input.txt" |> readFile |> Seq.mapi mapLines |> Seq.concat |> Seq.toList

let start = coords |> List.filter (fun e -> e.Position)
let goal = coords |> List.find (fun e -> e.Goal)
let edges = coords |> List.fold (getEdges coords) [] 

edges |> findShortestPath start |> (fun e -> e goal.Coordinates) |> printfn "Part1= %A"
printfn $"Part1 Elapsed: {Stopwatch.GetElapsedTime(startTime)}"

let startTimePart2 = Stopwatch.GetTimestamp()
let startPositions = coords |> List.filter (fun e -> e.Elevation = 0)
edges |> findShortestPath startPositions |> (fun e -> e goal.Coordinates) |> printfn "Part2= %A"

printfn $"Part 2Elapsed: {Stopwatch.GetElapsedTime(startTimePart2)}; Total Elapsed: {Stopwatch.GetElapsedTime(startTime)}"