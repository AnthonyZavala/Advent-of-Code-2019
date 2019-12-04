// Learn more about F# at http://fsharp.org

open System

let isSixDigitNumber (number: int) =
    number.ToString().Length = 6

let isWithinRange (min: int) (max: int) (number: int) =
    min <= number && number <= max

let hasTwoAdjacentDigits (number: int) =
    number.ToString().ToCharArray() |> Array.windowed 2 |> Array.exists (fun x -> x.[0] = x.[1])

let hasTwoAdjacentDigitsNotPartOfLargerGroupOfMatchingDigits (number: int) =
    let charArray = number.ToString().ToCharArray()
    let adjacentDigits = charArray |> Array.windowed 2 |> Array.filter (fun x -> x.[0] = x.[1]) |> Array.map (fun x -> x.[0])
    adjacentDigits |> Array.exists (fun x -> (adjacentDigits |> Array.filter (fun y -> x = y)).Length = 1)

let isIncreasingDigits (number: int) =
    let intArray = number.ToString().ToCharArray() |> Array.map (fun x -> Convert.ToInt32(x))
    let sortedIntArray = intArray |> Array.sort
    intArray = sortedIntArray

let meetsAllCriteria min max number =
    number |> isSixDigitNumber &&
    (min, max, number) |||> isWithinRange &&
    number |> hasTwoAdjacentDigits &&
    number |> isIncreasingDigits

let meetsAllPart2Criteria min max number =
    number |> isSixDigitNumber &&
    (min, max, number) |||> isWithinRange &&
    number |> hasTwoAdjacentDigitsNotPartOfLargerGroupOfMatchingDigits &&
    number |> isIncreasingDigits

let getNumberOfPossiblities min max =
    let range = [ min .. max ]
    range |> List.filter (fun number -> (min, max, number) |||> meetsAllCriteria) |> List.length 

let getNumberOfPart2Possiblities min max =
    let range = [ min .. max ]
    range |> List.filter (fun number -> (min, max, number) |||> meetsAllPart2Criteria) |> List.length 

[<EntryPoint>]
let main argv =
    let min = 193651
    let max = 649729

    let number = 112233

    // Part 1
    // printfn "isSixDigitNumber: %b" (number |> isSixDigitNumber)
    // printfn "isWithinRange: %b" ((min, max, number) |||> isWithinRange)
    // printfn "hasTwoAdjacentDigits: %b" (number |> hasTwoAdjacentDigits)
    // printfn "isIncreasingDigits: %b" (number |> isIncreasingDigits)
    // printfn "meetsAllCriteria: %b" ((min, max, number) |||> meetsAllCriteria)

    printfn "Number of possiblities: %i" ((min, max) ||> getNumberOfPossiblities)

    // Part 2
    // printfn "hasTwoAdjacentDigitsNotPartOfLargerGroupOfMatchingDigits: %b" (number |> hasTwoAdjacentDigitsNotPartOfLargerGroupOfMatchingDigits)

    printfn "Number of possiblities: %i" ((min, max) ||> getNumberOfPart2Possiblities)


    0 // return an integer exit code
