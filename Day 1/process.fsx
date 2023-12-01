// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"Day 1\input-data.txt"

let extractFirstAndLastNumbers cs =
    let tensVal = 
        cs 
        |> Array.find (fun c -> fst (System.Int32.TryParse (string c)))
        |> fun c -> let _, parsed = System.Int32.TryParse (string c)
                    parsed

    let unitsVal =
        cs 
        |> Array.findBack (fun c -> fst (System.Int32.TryParse (string c)))    
        |> fun c -> let _, parsed = System.Int32.TryParse (string c)
                    parsed

    (tensVal * 10) + unitsVal

let ss = ReadData.readLines filePath
         |> Seq.toArray
         |> Seq.map (fun line -> line.ToCharArray())
         |> Seq.map (extractFirstAndLastNumbers)
         |> Seq.sum


printfn "%A" ss