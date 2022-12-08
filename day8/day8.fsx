type Tree = {Value: int; Row: int; Column: int; Edge: bool}

let readFile filePath = System.IO.File.ReadLines filePath

let inline charToInt c = int c - int '0'

let mapColumn row index tree = {Value=charToInt tree; Row=row; Column=index; Edge=false}
let mapRows index (row: string) = row |> Seq.mapi (mapColumn index) 
let mapEdges right bottom tree = match tree with | _ when tree.Row = 0 || tree.Row = bottom || tree.Column = 0 || tree.Column = right -> {tree with Edge=true} | _ -> tree

let isCovered filterFn tree = Seq.filter filterFn >> Seq.exists (fun t -> t.Value >= tree.Value)

let treeCovered trees tree =
    let coveredTop = trees |> isCovered (fun t -> t.Column = tree.Column && t.Row < tree.Row) tree
    let coveredBottom = trees |> isCovered (fun t -> t.Column = tree.Column && t.Row > tree.Row) tree
    let coveredLeft = trees |> isCovered (fun t -> t.Row = tree.Row && t.Column < tree.Column) tree
    let coveredRight = trees |> isCovered (fun t -> t.Row = tree.Row && t.Column > tree.Column) tree
    coveredTop && coveredBottom && coveredLeft && coveredRight
    
let createdTrees = "input.txt" |> readFile |> Seq.mapi mapRows |> Seq.concat

let getMaxBy selector = Seq.maxBy selector >> selector
let rightColumn = createdTrees |> getMaxBy (fun t -> t.Column)
let bottomRow = createdTrees |> getMaxBy (fun t -> t.Row)

let trees = createdTrees |> Seq.map (mapEdges rightColumn bottomRow)

let covered = trees |> Seq.filter (fun t -> not t.Edge) |> Seq.filter (treeCovered trees) |> Seq.length
trees |> Seq.length |> (fun l -> l-covered) |> printfn "Part1 = %A"