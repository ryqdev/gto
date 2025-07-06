type card_value =
    | Ace
    | King
    | Queen
    | Jack
    | Number of int

type card_suit =
    | Spade
    | Heart
    | Club
    | Diamond

type card =
    { value: card_value
    ; suit: card_suit
    }

type deck =
    { cards: card array
     ; card_index: int
    }

let deal_top_n_card n deck =
    if deck.card_index + n <= Array.length deck.cards then
        let cards = Array.sub deck.cards deck.card_index n in
        let new_deck = {deck with card_index = deck.card_index + n} in
        Some (cards, new_deck)
    else
        None

let string_of_card_value = function
    | Ace -> "A"
    | King -> "K"
    | Queen -> "Q"
    | Jack -> "J"
    | Number n -> string_of_int n

let color_reset = "\027[0m"
let color_red = "\027[31m"
let color_white = "\027[37m"

let string_of_card_suit = function
    | Spade -> color_white ^ "♠" ^ color_reset
    | Heart -> color_red ^ "♥" ^ color_reset
    | Club -> color_white ^ "♣" ^ color_reset
    | Diamond -> color_red ^ "♦" ^ color_reset

let string_of_card card =
    Printf.sprintf "%s%s"
        (string_of_card_value card.value)
        (string_of_card_suit card.suit)

(* Fisher-Yates Shuffle Algorithm *)
let shuffle deck =
    let updated_cards = Array.copy deck.cards in
    for i = 0 to 51 do
        let j = Random.int (52 - i) + i in
        let temp = updated_cards.(i) in
        updated_cards.(i) <- updated_cards.(j);
        updated_cards.(j) <- temp
    done;
    {deck with cards = updated_cards}

let create_ordered_deck =
        let suits = [
            Spade; Heart; Club; Diamond
        ] in

        let values = [
            Ace; Number 2; Number 3; Number 4; Number 5; Number 6;
            Number 7; Number 8; Number 9; Number 10; Jack; Queen; King
        ] in

        let cards =
            List.map (fun suit ->
                List.map (fun value ->
                    {value; suit}
                ) values
            ) suits
            |> List.concat
            |> Array.of_list
        in
        {
            cards;
            card_index = 0;
        }
