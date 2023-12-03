// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"Day 1\input-data.txt"

open System

type Dirn = Forward | Backward

type String with
    member __.Reverse() =
        __ |> Seq.rev |> Seq.fold (fun acc c -> acc + (string c)) ""

let numberMatch dirn digit (numberRep: string) (input:string) =
    let fwdRep, bwdRep = numberRep, (numberRep.Reverse())

    input[0] = digit 
    || (dirn = Forward && input.StartsWith(fwdRep))
    || (dirn = Backward && input.StartsWith(bwdRep))

let (|One|_|) dirn (input: string) =    if numberMatch dirn '1' "one"   input then Some 1 else None
let (|Two|_|) dirn (input: string) =    if numberMatch dirn '2' "two"   input then Some 2 else None
let (|Three|_|) dirn (input: string) =  if numberMatch dirn '3' "three" input then Some 3 else None
let (|Four|_|) dirn (input: string) =   if numberMatch dirn '4' "four"  input then Some 4 else None
let (|Five|_|) dirn (input: string) =   if numberMatch dirn '5' "five"  input then Some 5 else None
let (|Six|_|) dirn (input: string) =    if numberMatch dirn '6' "six"   input then Some 6 else None
let (|Seven|_|) dirn (input: string) =  if numberMatch dirn '7' "seven" input then Some 7 else None
let (|Eight|_|) dirn (input: string) =  if numberMatch dirn '8' "eight" input then Some 8 else None
let (|Nine|_|) dirn (input: string) =   if numberMatch dirn '9' "nine"  input then Some 9 else None

let rec findNumber dirn (line: string) =
    match line with
    | line when line.Length = 0 -> failwith "No number like string found"
    | One dirn n -> n
    | Two dirn n -> n
    | Three dirn n -> n
    | Four dirn n -> n
    | Five dirn n -> n
    | Six dirn n -> n
    | Seven dirn n -> n
    | Eight dirn n -> n
    | Nine dirn n -> n
    | _ -> findNumber dirn line[1..]

let part1Result (line: string) =
    let tensVal = 
        line.ToCharArray() 
        |> Array.find (fun c -> fst (System.Int32.TryParse (string c)))
        |> fun c -> let _, parsed = System.Int32.TryParse (string c)
                    parsed

    let unitsVal =
        line.ToCharArray() 
        |> Array.findBack (fun c -> fst (System.Int32.TryParse (string c)))    
        |> fun c -> let _, parsed = System.Int32.TryParse (string c)
                    parsed

    (tensVal * 10) + unitsVal

let part2Result (line : string) =
    let tensVal = line |> findNumber Forward
    let unitsVal = line.Reverse() |> findNumber Backward

    (tensVal * 10) + unitsVal

let run extractor = 
        ReadData.readLines filePath
        |> Seq.toArray
        |> Seq.map (extractor)
        |> Seq.sum
    
printfn "Part1: %A" (part1Result |> run)
printfn "Part2: %A" (part2Result |> run)