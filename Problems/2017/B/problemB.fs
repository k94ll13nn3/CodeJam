module ProblemB

open System

let rec isTidy (number : list<char>) =
    match number with
    | [] -> true
    | [_] -> true
    | [a; b] -> a <= b
    | h::t -> let b = t.Head in h <= b && isTidy t

let getCaseResult index value =
    sprintf "Case #%d: %s" (index + 1) value

let numberToCharList n =
    [for c in n.ToString() -> c]

let rec getNextNumberEx (number : list<char>) = 
    match number with
    | [] -> []
    | [c] -> [(int c) - 1 |> char]
    | [a; b] -> if a > b || b = '0' then [(int a) - 1 |> char; '9'] else if a = '0' then [a; b] else [a; (int b) - 1 |> char]
    | a::t -> let b = t.Head in 
                if a > b then
                    ((int a) - 1 |> char)::(List.init (number.Length - 1) (fun i -> '9'))
                else
                    a::(getNextNumberEx t)

let getNextNumber (number : uint64) = 
    number 
    |> numberToCharList 
    |> getNextNumberEx
    |> List.toArray 
    |> String 
    |> uint64

let findHigherTidyNumber n =
    Seq.unfold (fun i -> Some((i, getNextNumber i))) n
    |> Seq.map numberToCharList
    |> Seq.find isTidy
    |> List.toArray
    |> String

let parseInput (input : seq<string>) =
    input
    |> Seq.skip 1
    |> Seq.map (uint64 >> findHigherTidyNumber)
    |> Seq.mapi getCaseResult

let resolve a =
    parseInput a