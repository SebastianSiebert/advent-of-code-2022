open System.Text.RegularExpressions

type File = {Name: string list; Size: int}
type Directory = {Name: string list; DirSize: int}

type Structure = | File of File | Dir of Directory
        
let readFile filePath = System.IO.File.ReadLines filePath
let regexCd = Regex "\$ cd (.+)"
let regexFile = Regex "(\d+) (.+)"

let changeDirectory (currentDir, structures) (regex: Match) =
    let dirName = regex.Groups[1].Value
    match dirName with
    | ".." -> (List.tail currentDir, structures)
    | _ ->
        let newCurDir = dirName::currentDir
        (newCurDir, Dir {Name = newCurDir; DirSize=0}::structures)
        
let addFile (currentDir, structures) (regex: Match) = (currentDir, File {Name=regex.Groups[2].Value::currentDir; Size=int regex.Groups[1].Value}::structures)

let createFileTree state line =
    let cd = regexCd.Match line
    let file = regexFile.Match line
    if cd.Success then changeDirectory state cd
    elif file.Success then addFile state file
    else state

let (|IsFile|) input = match input with | File m -> Some m | _ -> None
let (|IsDir|) input = match input with | Dir m -> Some m | _ -> None
let chooseStructure (|IsStructure|_|) value = match value with | IsStructure t -> Some t | _ -> None

let filterFilesDirAndSubDir dir (file: File) =
    let fileDir = file.Name.Tail
    if fileDir.Length < dir.Name.Length then false
    elif fileDir.Length > dir.Name.Length then List.forall2 (=) fileDir[fileDir.Length-dir.Name.Length..] dir.Name
    else List.forall2 (=) fileDir dir.Name

let calculateDirSize (files: File list) dir =
    let size = files |> List.filter (filterFilesDirAndSubDir dir) |> List.sumBy (fun e -> e.Size)
    {dir with DirSize=size}
    
let getStructures = "input.txt" |> readFile |> Seq.fold createFileTree ([], []) |> snd
let files = getStructures |> List.choose (|IsFile|)
let directories = getStructures |> List.choose (|IsDir|) |> List.map (calculateDirSize files)

directories |> List.filter (fun d -> d.DirSize < 100000) |> List.sumBy (fun d -> d.DirSize) |> printfn "Part1 = %i"

let totalDiskSpace = 70000000
let spaceNeeded = 30000000
let spaceUsed = directories |> List.find (fun d -> d.Name = ["/"]) |> (fun d -> d.DirSize)
let unusedSpace = totalDiskSpace - spaceUsed
let neededToFreeSpace = spaceNeeded - unusedSpace

let directorySpaceToDelete = directories |> List.filter (fun d -> d.DirSize > neededToFreeSpace) |> List.map (fun d -> d.DirSize) |> List.min
directorySpaceToDelete |> printfn "Part2 = %A"
