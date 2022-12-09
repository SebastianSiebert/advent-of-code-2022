type Direction = | Right | Left | Up | Down
type Move = {Direction: Direction; Steps: int}
let moveValue direction = match direction with | Right -> (1,0) | Left -> (-1,0) | Up -> (0,1) | Down -> (0,-1)

let readFile filePath = System.IO.File.ReadLines filePath

let mapInput (input: string) =
    let parts = input.Split " "
    {Direction=(match parts[0] with | "R" -> Right | "L" -> Left | "U" -> Up | _ -> Down); Steps=int parts[1]}
    
let calculateKnot (hx,hy) (tx,ty) =
    let difX = hx - tx
    let difY = hy - ty
    match (difX,difY) with
    | _ when difX >= -1 && difX <= 1 && difY >= -1 && difY <= 1 -> (tx,ty)
    | _ when difX = 0 || difY = 0 ->
        let x = match difX with | _ when difX > 1 -> tx+1 | _ when difX < -1 -> tx-1 | _ -> tx
        let y = match difY with | _ when difY > 1 -> ty+1 | _ when difY < -1 -> ty-1 | _ -> ty
        (x,y)
    | _ ->
        let x = match difX with | _ when difX >= 1 -> tx+1 | _ when difX <= -1 -> tx-1 | _ -> tx
        let y = match difY with | _ when difY >= 1 -> ty+1 | _ when difY <= -1 -> ty-1 | _ -> ty
        (x,y)

let calculateHead direction (x,y) =
    let x',y' = moveValue direction
    (x+x',y+y')
    
let foldKnots (prevKnot, knots) knot =
    let newKnot = calculateKnot prevKnot knot
    (newKnot, newKnot::knots)
    
let moveStep direction (tailPositions,head,tail) _ =
    let newHead = calculateHead direction head
    let newTail = calculateKnot newHead tail
    (newTail::tailPositions,newHead,newTail)
    
let moveStepLong direction (tailPositions,head,knots) _ =
    let newHead = calculateHead direction head
    let newKnots = knots |> List.fold foldKnots (newHead,[]) |> snd |> List.rev
    ((newKnots |> List.rev |> List.head)::tailPositions,newHead,newKnots)
    
let makeMoves stepFn state (move: Move) = [1..move.Steps] |> List.fold (stepFn move.Direction) state

let moves = "input.txt" |> readFile |> Seq.map mapInput
moves |> Seq.fold (makeMoves moveStep) ([(0,0)],(0,0),(0,0)) |> fun (tail,_,_) -> tail |> List.distinct |> List.length |> printfn "Part1 = %i"

let knots = [1..9] |> List.map (fun _ -> (0,0)) 
moves |> Seq.fold (makeMoves moveStepLong) ([0,0],(0,0),knots) |> fun (tail,_,_) -> tail |> List.distinct |> List.length |> printfn "Part2 = %i"
