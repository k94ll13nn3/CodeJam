module ProblemA

type Cookie =
    | Happy
    | Sad

let flipSide cookie =
    match cookie with
    | Happy -> Sad
    | Sad -> Happy

let flipRow cookies index length =
    cookies
    |> Seq.mapi (fun i cookie -> if i >= index && i < index + length then flipSide cookie else cookie)

let getIndexOfFirstSad cookies =
    cookies
    |> Seq.tryFindIndex (fun c -> c = Sad)

let computeNumberOfFlips cookies length =
    let rec computeNextFlip totalNumberOfFlips currentCookies =
        let i = getIndexOfFirstSad currentCookies
        match i with
        | None -> totalNumberOfFlips
        | Some index -> 
            if index + length > (cookies |> Seq.length) then
                -1
            else 
                flipRow currentCookies index length |> computeNextFlip (totalNumberOfFlips + 1)
    computeNextFlip 0 cookies

let parseCookies s =
    s
    |> Seq.map (fun c -> if c = '+' then Happy else Sad)

let getCaseResult index value =
    match value with
    | -1 -> sprintf "Case #%d: IMPOSSIBLE" (index + 1)
    | _ -> sprintf "Case #%d: %d" (index + 1) value

let parseInput (input : seq<string>) =
    input
    |> Seq.skip 1
    |> Seq.map (fun s -> s.Split(' '))
    |> Seq.map (fun s -> computeNumberOfFlips (s.[0] |> parseCookies) (int s.[1]))
    |> Seq.mapi (fun index value -> getCaseResult index value)

let resolve a =
    parseInput a