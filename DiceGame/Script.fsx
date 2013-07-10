// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open DiceGame

// Farkle game rules encoded in F#
// Released into the public domain
type RollResult =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

let countFives ds =
    let fiveCount = ds |> List.filter (fun d -> d = Five) |> List.length
    let others = ds |> List.filter (fun d -> not(d = Five))
    (fiveCount, others)

let countOnes ds =
    let oneCount = ds |> List.filter (fun d -> d = One) |> List.length
    let others = ds |> List.filter (fun d -> not(d = One))
    (oneCount, others)

let hasFourOfAnyNumber ds =
    match ds |> List.sort with
    | [a;b;c;d;e;f] when a = b && b = c && c = d && not(d = e) && not(e = f) -> (true, [e;f])
    | [a;b;c;d;e;f] when b = c && c = d && d = e && not(a = b) && not(e = f) -> (true, [a;f])
    | [a;b;c;d;e;f] when c = d && d = e && e = f && not(a = b) && not(b = c) -> (true, [a;b])
    | [a;b;c;d;e] when a = b && b = c && c = d && not(d = e) -> (true, [e])
    | [a;b;c;d;e] when b = c && c = d && d = e && not(a = b) -> (true, [a])
    | x -> (false, x)

let hasFiveOfAnyNumber ds =
    match ds |> List.sort with
    | [a;b;c;d;e;f] when a = b && b = c && c = d && d = e && not(a = f) -> (true, [f])
    | [a;b;c;d;e;f] when b = c && c = d && d = e && e = f && not(a = f) -> (true, [a])
    | [a;b;c;d;e] when a = b && b = c && c = d && d = e -> (true, [])
    | x -> (false, x)

let hasSixOfAnyNumber ds =
    match ds |> List.sort with
    | [a;b;c;d;e;f] when a = b && b = c && c = d && d = e && e = f -> (true, [])
    | x -> (false, x)

let isStraight ds =
    match ds |> List.sort with
    | [One; Two; Three; Four; Five; Six] -> (true, [])
    | x -> (false, x)

let hasThreePair ds =
    match ds |> List.sort with
    | [a;a';b;b';c;c'] when a = a' && b = b' && c = c' && not(a = b) && not (b = c) && not (a = c) -> (true, [])
    | x -> (false, x)

let hasTwoTriplets ds =
    match ds |> List.sort with
    | [a; a'; a''; b; b'; b''] when (a = a' && a'= a'') && (b = b' && b' = b'') && not (a = b) -> (true, [])
    | x -> (false, x)
    
let hasFourWithAPair ds =
    match ds |> List.sort with
    | [a; a'; a''; a'''; b; b'] when a = a' && a' = a'' && a'' = a''' && b = b' && not (a = b) -> (true, [])
    | [b; b'; a; a'; a''; a'''] when a = a' && a' = a'' && a''= a''' && b = b' && not (a = b) -> (true, [])
    | x -> (false, x)

let score ds =
    match ds with
    | x when fst (hasTwoTriplets x) -> (2500, [])
    | x when fst (hasFourWithAPair x) -> (1500, [])
    | x when fst (hasThreePair x) -> (1500, [])
    | x when fst (isStraight x) -> (1500, [])
    | x when fst (hasSixOfAnyNumber x) -> (3000, [])

    | x when fst (hasFiveOfAnyNumber x) -> (2000, hasFiveOfAnyNumber x |> snd)
    | x when fst (hasFourOfAnyNumber x) -> (1000, hasFourOfAnyNumber x |> snd)
    | Six :: Six :: Six :: rs -> (600, rs)
    | Five :: Five :: Five :: rs -> (500, rs)
    | Four :: Four :: Four :: rs -> (400, rs)
    | Three :: Three :: Three :: rs -> (300, rs)
    | One :: One :: One :: rs -> (300,rs)
    | Two :: Two :: Two :: rs -> (200, rs)
    | x when fst (countOnes x) > 0 -> (countOnes x |> fst |> (fun n -> n * 100), countOnes x |> snd)
    | x when fst (countFives x) > 0 -> (countFives x |> fst |> (fun n -> n * 50), countFives x |> snd)
    | _ -> (0, [])

let toRollResult x = 
    match x with
    | 1 -> One
    | 2 -> Two
    | 3 -> Three
    | 4 -> Four
    | 5 -> Five
    | 6 -> Six
    | _ -> raise (System.NotImplementedException())

let score6 a b c d e f =
    let rs = [a;b;c;d;e;f] |> List.map toRollResult
    score rs

let score5 a b c d e =
    let rs = [a;b;c;d;e] |> List.map toRollResult
    score rs

let score4 a b c d  =
    let rs = [a;b;c;d] |> List.map toRollResult
    score rs

let score3 a b c =
    let rs = [a;b;c] |> List.map toRollResult
    score rs

let score2 a b =
    let rs = [a;b] |> List.map toRollResult
    score rs

let score1 a =
    let rs = [a] |> List.map toRollResult
    score rs

