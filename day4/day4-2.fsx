let readFile filePath = System.IO.File.ReadLines filePath
let regex = System.Text.RegularExpressions.Regex "(\d+)-(\d+),(\d+)-(\d+)"
let matchToValue (m: System.Text.RegularExpressions.Match) = (Set.ofSeq {int m.Groups[1].Value .. int m.Groups[2].Value}, Set.ofSeq {int m.Groups[3].Value .. int m.Groups[4].Value})
    
let processData filterFn template = readFile >> Seq.map regex.Match >> Seq.map matchToValue >> Seq.filter filterFn >> Seq.length >> printfn template
"input.txt" |> processData (fun (s1,s2) -> Set.isSubset s1 s2 || Set.isSuperset s1 s2) "Part1 = %i"
"input.txt" |> processData (fun (s1,s2) -> Set.intersect s1 s2 |> Set.isEmpty |> not) "Part2 = %i"
