let readLine filePath = System.IO.File.ReadAllText filePath

let calculateBufferSize markerLenght (state: int * char list * bool) char =
    match state with
    | _,_,finished when finished -> state
    | count,chars,finished when chars.Length < markerLenght ->  (count+1,char::chars,finished)
    | count,chars,finished ->
        let countOfDistinct = chars |> List.distinct |> List.length
        if countOfDistinct = markerLenght then (count,chars,true) else (count+1,char::chars[..chars.Length-2],finished)
    
"input.txt" |> readLine |> Seq.fold (calculateBufferSize 4) (0,[],false) |> printfn "Part1 = %A"
"input.txt" |> readLine |> Seq.fold (calculateBufferSize 14) (0,[],false) |> printfn "Part2 = %A"
