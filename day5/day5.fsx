open System.Text.RegularExpressions

type Move = { Number: int; FromStack: int; ToStack: int }
type StackDefinition = { Position: int; Index: int }

type Stack = { Position: int; Index: int; Stack: string list}

type Input =
    | Stack of (int * string) list
    | StackNumber of StackDefinition list
    | Moves of Move
    | Empty
    
let readFile filePath = System.IO.File.ReadLines filePath
let regexStack = Regex "([A-Z])"
let regexStackNo = Regex "(\d)"
let regexMoves = Regex "move (\d+) from (\d+) to (\d+)"

let matchMove (moveMatch: Match) = Moves {Number=int moveMatch.Groups[1].Value; FromStack=int moveMatch.Groups[2].Value; ToStack=int moveMatch.Groups[3].Value}
let matchStackNo (stackNoMatch: MatchCollection) =
    let mapStackNo index =
        let item = stackNoMatch.Item index
        { Position = item.Index; Index = int item.Value }
    let l = [0..stackNoMatch.Count-1] |> List.map mapStackNo
    StackNumber l
let matchStack (stackMatch: MatchCollection) =
    let mapStack index =
        let item = stackMatch.Item index
        (item.Index, item.Value)
    let l = [0..stackMatch.Count-1] |> List.map mapStack
    Stack l

let mapValues line =
    let move = regexMoves.Match line
    let stackNo = regexStackNo.Matches line
    let stack = regexStack.Matches line
    if move.Success then matchMove move
    elif stackNo.Count > 0 then  matchStackNo stackNo
    elif stack.Count > 0 then matchStack stack
    else Empty
    
let createEmptyStacks (stackDefinition: StackDefinition list) =
    let stacks = stackDefinition |> List.fold (fun state def -> {Position=def.Position; Index=def.Index; Stack=[]}::state) []
    stacks
    
let fillStacks stacks stackContent =
    let mapStack s =
        let stack = List.tryFind (fun (p,_) -> p = s.Position) stackContent
        match stack with
        | None -> s
        | Some (_,v) ->
            { s with Stack = v::s.Stack }
            
    stacks  |> List.map mapStack

let makeMoves fn stacks move =
    let stack = List.find (fun e -> e.Index = move.FromStack) stacks
    let mapStack s =
        match s.Index with
        | _ when s.Index = move.FromStack -> {s with Stack = s.Stack[move.Number..] }
        | _ when s.Index = move.ToStack -> {s with Stack = (List.append (fn stack.Stack[..move.Number-1]) s.Stack)}
        | _ -> s
    stacks |> List.map mapStack
    
    
let processMoves makeMovesFn stack = Seq.choose (fun e -> match e with | Moves m -> Some m | _ -> None) >> Seq.fold (makeMoves makeMovesFn) stack >> Seq.sortBy (fun s -> s.Index) >> Seq.map (fun e -> e.Stack.Head) >> System.String.Concat

let inputs = "input.txt" |> readFile |> Seq.map mapValues
let stackDefinition = inputs |> Seq.choose (fun e -> match e with | StackNumber sn -> Some sn | _ -> None) |> Seq.head
let stacks = inputs |> Seq.choose (fun e -> match e with | Stack s -> Some s | _ -> None) |> Seq.rev |> Seq.fold fillStacks (createEmptyStacks stackDefinition)

inputs |> processMoves List.rev stacks |> printfn "Part1 = %s"
inputs |> processMoves id stacks |> printfn "Part2 = %s"
