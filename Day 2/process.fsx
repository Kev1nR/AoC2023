#load @"..\Read-data.fsx"

open System
open System.Text.RegularExpressions

let filePath = @"Day 2\input-data.txt"

type RGB = {R:int; G:int; B:int}
type Game = { Game: int; Round: RGB list }

// sample input is, for example: "8 green, 6 blue, 20 red"
let toRGB input =
    let rec toRGB' rgb (next: string list) =
        match next with
        | [] -> rgb
        | h::t ->
            let count_colour = h.Trim().Split(" ")
            let rgb' = 
                if count_colour[1] = "red" then { rgb with R = int count_colour[0] }
                elif count_colour[1] = "green" then { rgb with G = int count_colour[0] }
                elif count_colour[1] = "blue" then { rgb with B = int count_colour[0] }
                else rgb
            toRGB' rgb' t
    toRGB' {R=0;G=0;B=0} input 

let run () =
    ReadData.readLines filePath
    |> Seq.map (fun line -> 
                    let games_and_cubes = line.Split (":")
                    let gameId = int (games_and_cubes[0].Split(" ")[1])
                    
                    let rounds = 
                        games_and_cubes[1].Trim().Split(";") 
                        |> Array.toList 
                        |> List.map (fun pick -> pick.Split(",") |> Array.toList |> toRGB)
                    {Game = gameId; Round=rounds}
               )

let part1Result () =
    run()
    |> Seq.filter (fun g -> g.Round |> List.forall (fun r -> r.R <= 12 && r.G <= 13 && r.B <= 14))
    |> Seq.sumBy (fun g -> g.Game)

let part2Result () =
    run ()
    |> Seq.map (fun g -> 
                    let redMax = g.Round |> List.maxBy (fun r -> r.R) |> fun r -> r.R
                    let greenMax = g.Round |> List.maxBy (fun r -> r.G) |> fun r -> r.G
                    let blueMax = g.Round |> List.maxBy (fun r -> r.B) |> fun r -> r.B                   
                    redMax * greenMax *blueMax
               )
    |> Seq.sum

printfn "%A" (part1Result())        
printfn "%A" (part2Result())     


let test_toRGB () = 
    "8 green, 6 blue, 20 red".Split(",")
    |> Array.toList
    |> toRGB

