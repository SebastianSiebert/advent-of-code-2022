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
    | ".." ->
        let newCurDir = List.tail currentDir
        (newCurDir, structures)
    | _ ->
        let newCurDir = dirName::currentDir
        (newCurDir, Dir {Name = newCurDir; DirSize=0}::structures)
        
let addFile (currentDir, structures) (regex: Match) =
    let fileSize = int regex.Groups[1].Value
    let fileName = regex.Groups[2].Value
    (currentDir, File {Name=fileName::currentDir; Size=fileSize}::structures)

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
    // let size = files |> List.filter (fun e -> e.Name.Tail = dir.Name) |> List.sumBy (fun e -> e.Size)
    {dir with DirSize=size}
    
let filterDirSizeLess size dir =
    if dir.DirSize < size then true
    else false
    
let getStructures = "input.txt" |> readFile |> Seq.fold createFileTree ([], []) |> snd
let files = getStructures |> List.choose (|IsFile|)
let directories =
    getStructures
    |> List.choose (|IsDir|)
    |> List.map (calculateDirSize files)
    |> List.sortByDescending (fun e -> e.Name)

directories |> List.filter (fun d -> d.DirSize < 100000) |> List.sumBy (fun d -> d.DirSize) |> printfn "Part1 = %i"
