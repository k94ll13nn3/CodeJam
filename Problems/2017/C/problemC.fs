module ProblemC

type Block = {Size : int; Start : int; End : int}

let getCaseResult index value =
    sprintf "Case #%d: %d %d" (index + 1) (fst value) (snd value)

let getIndexAndMinimumAndMaximum (stalls : seq<int>) =
    let biggestBlock = 
        stalls 
        |> Seq.fold (fun acc elem -> let (last, lst) = acc in (elem, { Size = elem - last - 1; Start = last; End = elem }::lst))  (0, [])
        |> snd
        |> List.maxBy (fun b -> (b.Size, -b.Start))
    let firstPart = biggestBlock.Size / 2
    let otherPart = biggestBlock.Size - firstPart - 1
    (biggestBlock.Start + (min firstPart otherPart) + 1, firstPart, otherPart)
    
let getLastMinimumAndMaximum numberOfStalls numberOfPeople =
    if numberOfStalls = numberOfPeople then
        (0, 0)
    else
        let rec computeNextPeople nextPeople currentStalls  =
            let (index, max, min) = getIndexAndMinimumAndMaximum currentStalls
            if nextPeople = numberOfPeople - 1 then
                (max, min)
            else
                computeNextPeople (nextPeople + 1) ((index::currentStalls) |> List.sort)
        computeNextPeople 0 [0; numberOfStalls + 1] 

let parseInput (input : seq<string>) =
    input
    |> Seq.skip 1
    |> Seq.map (fun s -> s.Split(' '))
    |> Seq.map (fun s -> getLastMinimumAndMaximum (int s.[0]) (int s.[1]))
    |> Seq.mapi (fun index value -> getCaseResult index value)

let resolve a =
    parseInput a