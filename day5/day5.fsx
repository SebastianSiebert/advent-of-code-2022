open System.Text.RegularExpressions

type Move = { Number: int; FromStack: int; ToStack: int }

type Input =
    | Stack of (int * string) list
    | StackNumber of (int * string) list
    | Moves of Move
    | Empty
    
let readFile filePath = System.IO.File.ReadLines filePath
let regexStack = Regex "([A-Z])"
let regexStackNo = Regex "(\d)"
let regexMoves = Regex "move (\d+) from (\d+) to (\d+)"

let matchMove (moveMatch: Match) =
    Moves {Number=int moveMatch.Groups[1].Value; FromStack=int moveMatch.Groups[2].Value; ToStack=int moveMatch.Groups[3].Value}
let matchStackNo (stackNoMatch: MatchCollection) =
    let mapStackNo index =
        let item = stackNoMatch.Item index
        (item.Index, item.Value)
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
    
type Stack = {Index: string; Stack: string list}

let createStacks stackDefinition =
    List.map (fun (_, value) -> {Index= value; Stack= []}) stackDefinition

let mapStacks stackDefinition state stackRow =
    
    List.fold (fun e -> e)
    state

let inputs = "example.txt" |> readFile |> Seq.map mapValues
let stackDefinition = inputs |> Seq.find (fun e -> match e with | StackNumber _ -> true | _ -> false) |> (fun (StackNumber e) -> e)

let stacks = inputs |> Seq.filter (fun e -> match e with | Stack _ -> true | _ -> false)|> Seq.map (fun (Stack e) -> e) |> Seq.rev |> Seq.fold (mapStacks stackDefinition) (createStacks stackDefinition)
let moves = inputs |> Seq.filter (fun e -> match e with | Moves _ -> true | _ -> false)

printfn "%A" stackDefinition