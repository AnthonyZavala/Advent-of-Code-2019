open System

let rec runProgram program i =
    let programLine = program |> List.skip i |> List.truncate 4
    match programLine.[0] with
    | 1 -> (program |> List.mapi (fun i int -> if (i = programLine.[3]) then program.[programLine.[1]] + program.[programLine.[2]] else int), i + 4) ||> runProgram
    | 2 -> (program |> List.mapi (fun i int -> if (i = programLine.[3]) then program.[programLine.[1]] * program.[programLine.[2]] else int), i + 4) ||> runProgram
    | 99 -> program
    | _ -> printfn "Something went wrong"
           program

let rec findNounAndVerb (program: int List) target =
    if (((program, 0) ||> runProgram).[0] = target) then
        100 * program.[1] + program.[2]
    else if (program.[2] >= 99) then
        let newProgram = program |> List.mapi (fun i x -> if (i = 1) then x + 1 else if (i = 2) then 0 else x)
        (newProgram, target) ||> findNounAndVerb
    else
        let newProgram = program |> List.mapi (fun i x -> if (i = 2) then x + 1 else x)
        (newProgram, target) ||> findNounAndVerb

[<EntryPoint>]
let main argv =
    let program = [ 1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;9;19;1;13;19;23;2;23;9;27;1;6;27;31;2;10;31;35;1;6;35;39;2;9;39;43;1;5;43;47;2;47;13;51;2;51;10;55;1;55;5;59;1;59;9;63;1;63;9;67;2;6;67;71;1;5;71;75;1;75;6;79;1;6;79;83;1;83;9;87;2;87;10;91;2;91;10;95;1;95;5;99;1;99;13;103;2;103;9;107;1;6;107;111;1;111;5;115;1;115;2;119;1;5;119;0;99;2;0;14;0 ]
    
    // Part 1
    let part1Program = program |> List.mapi (fun i x -> if (i = 1) then 12 else if (i = 2) then 2 else x)
    printfn "%A" ((part1Program, 0) ||> runProgram)

    // Part 2
    printfn "%i" ((program, 19690720) ||> findNounAndVerb)

    0 // return an integer exit code
