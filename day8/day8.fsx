type Tree = {Value: int; Row: int; Column: int; Edge: bool}

module InputHelper =
    let readFile filePath = System.IO.File.ReadLines filePath

    let inline charToInt c = int c - int '0'

    let mapColumn row index tree = {Value=charToInt tree; Row=row; Column=index; Edge=false}
    let mapRows index (row: string) = row |> Seq.mapi (mapColumn index) 
    let mapEdges right bottom tree = match tree with | _ when tree.Row = 0 || tree.Row = bottom || tree.Column = 0 || tree.Column = right -> {tree with Edge=true} | _ -> tree

open InputHelper

let filterTrees filterFn = Seq.filter filterFn
let isCovered filterFn tree = filterTrees filterFn >> Seq.exists (fun t -> t.Value >= tree.Value)
let getTopTrees tree = fun t -> t.Column = tree.Column && t.Row < tree.Row
let getBottomTrees tree = fun t -> t.Column = tree.Column && t.Row > tree.Row
let getLeftTrees tree = fun t -> t.Row = tree.Row && t.Column < tree.Column
let getRightTrees tree = fun t -> t.Row = tree.Row && t.Column > tree.Column

let treeCovered trees tree =
    let coveredTop = trees |> isCovered (getTopTrees tree) tree
    let coveredBottom = trees |> isCovered (getBottomTrees tree) tree
    let coveredLeft = trees |> isCovered (getLeftTrees tree) tree
    let coveredRight = trees |> isCovered (getRightTrees tree) tree
    coveredTop && coveredBottom && coveredLeft && coveredRight
    
let trees = "input.txt" |> readFile |> Seq.mapi mapRows |> Seq.concat

let getMaxBy selector = Seq.maxBy selector >> selector
let rightColumn = trees |> getMaxBy (fun t -> t.Column)
let bottomRow = trees |> getMaxBy (fun t -> t.Row)

let takeTrees value state tree = match state with | _,finished when finished -> state | trees,_ -> (trees+1, tree.Value >= value)

let getScenicScore getTreesFn tree seqFn = Seq.filter (getTreesFn tree) >> seqFn >> Seq.fold (takeTrees tree.Value) (0,false) >> fst

let calculateScenicScore trees tree =
    let topScore = trees |> getScenicScore getTopTrees tree Seq.rev
    let bottomScore = trees |> getScenicScore getBottomTrees tree id
    let leftScore = trees |> getScenicScore getLeftTrees tree Seq.rev
    let rightScore = trees |> getScenicScore getRightTrees tree id
    topScore * bottomScore * leftScore * rightScore

let part1 = async{
    let innerTrees = trees |> Seq.map (mapEdges rightColumn bottomRow)

    let covered = innerTrees |> Seq.filter (fun t -> not t.Edge) |> Seq.filter (treeCovered innerTrees) |> Seq.length
    innerTrees |> Seq.length |> (fun l -> l-covered) |> printfn "Part1 = %i"
    }

let part2 = async { trees |> Seq.map (calculateScenicScore trees) |> Seq.max |> printfn "Part2 = %i" }

[part1;part2] |> Async.Parallel |> Async.RunSynchronously