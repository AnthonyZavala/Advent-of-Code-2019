// Learn more about F# at http://fsharp.org

open System

type Opcode = 
    | Addition
    | Multiplication
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | Exit
    static member Parse int = 
        match int with
        | 1 -> Addition
        | 2 -> Multiplication
        | 3 -> Input
        | 4 -> Output
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
        | 99 -> Exit
        | _ -> failwithf "Incorrect Opcode %i" int

type ParameterMode =
    | Position
    | Immediate
    static member Parse int =
        match int with
        | 0 -> Position
        | 1 -> Immediate
        | _ -> failwithf "Incorrect Parameter Mode %i" int

type Instructions = 
    { opcode: Opcode
      parameterModes: ParameterMode array }
      static member Parse int =
        let maxInstructionLength = 5
        let charInstructions = int.ToString().PadLeft(maxInstructionLength).ToCharArray()
        let opcode =
            ("", charInstructions.[charInstructions.Length - 2..])
            |>  String.Join
            |> Int32.Parse
            |> Opcode.Parse
        let parameterModes =
            charInstructions.[..charInstructions.Length - 3]
            |> Array.map (fun x -> (if x = ' ' then '0' else x).ToString() |> (Int32.Parse >> ParameterMode.Parse) )
            |> Array.rev
        { opcode = opcode; parameterModes = parameterModes }

let parameterModeIndex mode (program: int array) index =
    match mode with
    | Position -> program.[index]
    | Immediate -> index

let rec runProgram (program: int array) index =
    let instructions = program.[index] |> Instructions.Parse
    let getIndex number = parameterModeIndex (instructions.parameterModes.[number - 1]) program (index + number)
    match instructions.opcode with
    | Addition ->
        (program.[getIndex 1] + program.[getIndex 2]) |> Array.set program (getIndex 3)
        (program, (index + 4)) ||> runProgram
    | Multiplication ->
        (program.[getIndex 1] * program.[getIndex 2]) |> Array.set program (getIndex 3)
        (program, (index + 4)) ||> runProgram
    | Input ->
        printfn "Input: "
        Console.ReadLine() |> Int32.Parse |> Array.set program (getIndex 1)
        (program, index + 2) ||> runProgram
    | Output ->
        printfn "Output: %i" program.[getIndex 1]
        (program, index + 2) ||> runProgram
    | JumpIfTrue ->
        (program, if program.[getIndex 1] <> 0 then program.[getIndex 2] else index + 3) ||> runProgram
    | JumpIfFalse ->
        (program, if program.[getIndex 1] = 0 then program.[getIndex 2] else index + 3) ||> runProgram
    | LessThan ->
        let value = if program.[getIndex 1] < program.[getIndex 2] then 1 else 0
        value |> Array.set program (getIndex 3)
        (program, index + 4) ||> runProgram
    | Equals ->
        let value = if program.[getIndex 1] = program.[getIndex 2] then 1 else 0
        value |> Array.set program (getIndex 3)
        (program, index + 4) ||> runProgram
    | Exit ->
        program

[<EntryPoint>]
let main argv =
    // let program = [| 3;3;1105;-1;9;1101;0;0;12;4;12;99;1 |]
    let program = [| 3;225;1;225;6;6;1100;1;238;225;104;0;1101;81;30;225;1102;9;63;225;1001;92;45;224;101;-83;224;224;4;224;102;8;223;223;101;2;224;224;1;224;223;223;1102;41;38;225;1002;165;73;224;101;-2920;224;224;4;224;102;8;223;223;101;4;224;224;1;223;224;223;1101;18;14;224;1001;224;-32;224;4;224;1002;223;8;223;101;3;224;224;1;224;223;223;1101;67;38;225;1102;54;62;224;1001;224;-3348;224;4;224;1002;223;8;223;1001;224;1;224;1;224;223;223;1;161;169;224;101;-62;224;224;4;224;1002;223;8;223;101;1;224;224;1;223;224;223;2;14;18;224;1001;224;-1890;224;4;224;1002;223;8;223;101;3;224;224;1;223;224;223;1101;20;25;225;1102;40;11;225;1102;42;58;225;101;76;217;224;101;-153;224;224;4;224;102;8;223;223;1001;224;5;224;1;224;223;223;102;11;43;224;1001;224;-451;224;4;224;1002;223;8;223;101;6;224;224;1;223;224;223;1102;77;23;225;4;223;99;0;0;0;677;0;0;0;0;0;0;0;0;0;0;0;1105;0;99999;1105;227;247;1105;1;99999;1005;227;99999;1005;0;256;1105;1;99999;1106;227;99999;1106;0;265;1105;1;99999;1006;0;99999;1006;227;274;1105;1;99999;1105;1;280;1105;1;99999;1;225;225;225;1101;294;0;0;105;1;0;1105;1;99999;1106;0;300;1105;1;99999;1;225;225;225;1101;314;0;0;106;0;0;1105;1;99999;8;226;677;224;1002;223;2;223;1006;224;329;1001;223;1;223;7;226;226;224;102;2;223;223;1006;224;344;101;1;223;223;108;677;677;224;1002;223;2;223;1006;224;359;101;1;223;223;1107;226;677;224;1002;223;2;223;1005;224;374;101;1;223;223;1008;677;226;224;1002;223;2;223;1005;224;389;101;1;223;223;1007;677;226;224;1002;223;2;223;1005;224;404;1001;223;1;223;1107;677;226;224;1002;223;2;223;1005;224;419;1001;223;1;223;108;677;226;224;102;2;223;223;1006;224;434;1001;223;1;223;7;226;677;224;102;2;223;223;1005;224;449;1001;223;1;223;107;226;226;224;102;2;223;223;1006;224;464;101;1;223;223;107;677;226;224;102;2;223;223;1006;224;479;101;1;223;223;1007;677;677;224;1002;223;2;223;1006;224;494;1001;223;1;223;1008;226;226;224;1002;223;2;223;1006;224;509;101;1;223;223;7;677;226;224;1002;223;2;223;1006;224;524;1001;223;1;223;1007;226;226;224;102;2;223;223;1006;224;539;101;1;223;223;8;677;226;224;1002;223;2;223;1006;224;554;101;1;223;223;1008;677;677;224;102;2;223;223;1006;224;569;101;1;223;223;1108;677;226;224;102;2;223;223;1005;224;584;101;1;223;223;107;677;677;224;102;2;223;223;1006;224;599;1001;223;1;223;1108;677;677;224;1002;223;2;223;1006;224;614;1001;223;1;223;1107;677;677;224;1002;223;2;223;1005;224;629;1001;223;1;223;108;226;226;224;1002;223;2;223;1005;224;644;101;1;223;223;8;226;226;224;1002;223;2;223;1005;224;659;101;1;223;223;1108;226;677;224;1002;223;2;223;1006;224;674;101;1;223;223;4;223;99;226 |]
    printfn "%A" ((program, 0) ||> runProgram)

    0 // return an integer exit code
