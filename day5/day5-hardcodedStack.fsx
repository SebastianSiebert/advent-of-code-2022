open System.Text.RegularExpressions

type Move = { Number: int; FromStack: int; ToStack: int }
type Stack = {Index: int; Stack: string list}

type Input =
    | Moves of Move
    | Empty
    
let exampleStack = [ {Index=1; Stack=["N";"Z"]}; {Index=2; Stack=["D";"C";"M"]}; {Index=3; Stack=["P"]} ]
let inputStack = [
    {Index=1; Stack=["P";"D";"Q";"R";"V";"B";"H";"F"]}
    {Index=2; Stack=["V";"W";"Q";"Z";"D";"L"]}
    {Index=3; Stack=["C";"P";"R";"G";"Q";"Z";"L";"H"]}
    {Index=4; Stack=["B";"V";"J";"F";"H";"D";"R"]}
    {Index=5; Stack=["C";"L";"W";"Z"]}
    {Index=6; Stack=["M";"V";"G";"T";"N";"P";"R";"J"]}
    {Index=7; Stack=["S";"B";"M";"V";"L";"R";"J"]}
    {Index=8; Stack=["J";"P";"D"]}
    {Index=9; Stack=["V";"W";"N";"C";"D"]}
]
    
let readFile filePath = System.IO.File.ReadLines filePath
let regexMoves = Regex "move (\d+) from (\d+) to (\d+)"

let mapValues line =
    let move = regexMoves.Match line
    if move.Success then Moves {Number=int move.Groups[1].Value; FromStack=int move.Groups[2].Value; ToStack=int move.Groups[3].Value}
    else Empty
    
let makeMoves fn stacks move =
    let stack = List.find (fun e -> e.Index = move.FromStack) stacks
    let mapStack s =
        match s.Index with
        | _ when s.Index = move.FromStack -> {s with Stack = s.Stack[move.Number..] }
        | _ when s.Index = move.ToStack -> {s with Stack = (List.append (fn stack.Stack[..move.Number-1]) s.Stack)}
        | _ -> s
    stacks |> List.map mapStack
    
let processData makeMovesFn stack = readFile >> Seq.map mapValues >> Seq.filter (fun e -> match e with | Moves _ -> true | _ -> false) >> Seq.map (fun (Moves m) -> m) >> Seq.fold (makeMoves makeMovesFn) stack >> Seq.map (fun e -> e.Stack.Head) >> System.String.Concat

"input.txt" |> processData List.rev inputStack |> printfn "Part1 = %s" 
"input.txt" |> processData id inputStack |> printfn "Part2 = %s"
