// Learn more about F# at http://fsharp.org

open System
open Input

type OrbitTree =
    | LeafOrbit of LeafOrbitInfo
    | InternalOrbit of InternalOrbitInfo
    member this.Name =
        match this with
        | LeafOrbit leafOrbit -> leafOrbit.name
        | InternalOrbit internalOrbit -> internalOrbit.name
and LeafOrbitInfo = { name: string }
and InternalOrbitInfo = { name: string; childOrbits: OrbitTree array }

let getNodeFromMap (map: Map<string, OrbitTree>) (tree: OrbitTree) =
    match (map.TryFind (tree.Name)) with
    | Some s -> s
    | None -> tree

let rec fillTree tree (map: Map<string, OrbitTree>) =
    match tree with
    | LeafOrbit leafOrbit ->
        LeafOrbit leafOrbit
    | InternalOrbit internalOrbit ->
        InternalOrbit { internalOrbit with
                            childOrbits = internalOrbit.childOrbits
                            |> Array.map (fun x -> ((map, x) ||> getNodeFromMap, map) ||> fillTree) }


let rec countOrbits tree index =
    match tree with
    | LeafOrbit _ ->
        index
    | InternalOrbit internalOrbit ->
        index + (internalOrbit.childOrbits |> Array.sumBy (fun tree -> (tree, index + 1) ||> countOrbits))

let rec getOrbits (tree: OrbitTree) name (orbits: string array) =
    if tree.Name = name then
        Some orbits
    else
        match tree with
        | LeafOrbit _ ->
            None
        | InternalOrbit internalOrbit ->
            internalOrbit.childOrbits
            |> Array.map (fun x -> getOrbits x name (orbits |> Array.append [| x.Name |]) )
            |> Array.tryFind (fun x -> x.IsSome)
            |> Option.map (fun x -> x.Value)

[<EntryPoint>]
let main argv =
    let map =
        input
        |> Array.map (fun x -> x.Split ")")
        |> Array.groupBy (fun x -> x.[0])
        |> Map.ofArray
        |> Map.map (fun key value -> 
            InternalOrbit 
                { name = key;
                  childOrbits = value |> Array.map (fun x -> LeafOrbit { name = x.[1] }) } )
    
    let com = map.["COM"]
    let tree = map |> fillTree com

    // Part 1
    printfn "Total number of direct and indirect orbits: %i" ((tree, 0) ||> countOrbits)

    // Part 2
    let initSet = [| "COM" |]
    let youOrbits = (getOrbits tree "YOU" initSet).Value |> Array.tail
    let santaOrbits = (getOrbits tree "SAN" initSet).Value |> Array.tail

    let junctionPoint = youOrbits |> Array.find (fun x -> santaOrbits |> (Array.exists (fun y -> x = y)))
    let youTransfers = (youOrbits, junctionPoint) |> Array.IndexOf
    let santaTransfers = (santaOrbits, junctionPoint) |> Array.IndexOf

    printfn "Minimum number of orbital transfers required: %i" (youTransfers + santaTransfers)

    0 // return an integer exit code
