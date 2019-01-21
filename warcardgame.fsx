type Rank = 
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type Suit = 
    | Hearts
    | Diamonds
    | Spades
    | Clubs

let deck = 
    let allRanks = [Ace ; King ; Queen; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two]
    let allSuits = [Hearts ; Diamonds ; Spades ; Clubs]
    List.allPairs allRanks allSuits

let shuffle deck = 
    let rnd = System.Random()
    deck |> List.sortBy (fun _ -> rnd.Next ())

let distribute n xs =
    xs 
    |> List.mapi (fun i x -> (i, x))
    |> List.groupBy (fun (i, _) -> i % n)
    |> List.map (snd >> List.map snd)

let hands = deck |> shuffle |> distribute 2