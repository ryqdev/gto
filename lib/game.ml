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
     ; card_index: int ref
    }

let deal_top_card deck =
    if !(deck.card_index) < Array.length deck.cards then
        let card = deck.cards.(!(deck.card_index)) in
        incr deck.card_index;
        Some card
    else
        None

type player_position =
    | BTN
    | SB
    | BB
    | UTG

(* type player_action = *)
(*    | Check *)
(*    | Raise *)
(*    | Call *)
(*    | Fold *)
(*    | Allin *)

type player =
    { position: player_position
    ; name: string
    ; hole_cards: (card * card) option
    ; chips: int
    }

(* type game_state = *)
(*    | Flop *)
(*    | Turn *)
(*    | River *)

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

let string_of_player_position = function
   | BTN -> "Button"
   | SB -> "Small Blind"
   | BB -> "Big Blind"
   | UTG -> "Under the Gun"

let string_of_player player =
    let card_str = match player.hole_cards with
        | None -> "No Cards"
        | Some (card1, card2) ->
            Printf.sprintf "%s %s"
                (string_of_card card1)
                (string_of_card card2)
    in
    Printf.sprintf "%s %s %s %s"
        (string_of_player_position player.position)
        player.name
        card_str
        (string_of_int player.chips)

let shuffle_cards deck = deck

let deal_cards_to_player deck players =
    let deal_round players round_count =
        Printf.printf "Dealing cards in round %d" round_count;
        List.map (fun p ->
            match deal_top_card deck with
            | Some (card) ->
                Printf.printf "%s gets %s\n" p.name (string_of_card card);
                let updated_hole_cards = match round_count with
                    | 1 -> Some (card, card)
                    | 2 -> (match p.hole_cards with
                        | Some (first_card, _) -> Some (first_card, card)
                        | None -> failwith "Player should have received first card in round 1"
                    )
                    | _ -> failwith ("Invalid round count: " ^ string_of_int round_count)
                in
                {p with hole_cards = updated_hole_cards}
            | None -> failwith "No Card in Deck"
        ) players
    in
    let round1 = deal_round players 1 in
    let round2 = deal_round round1 2 in
    round2


(* let deal_community_cards cards card_index = *)
(*    Printf.printf "Dealing community cards...\n"; *)


let create player_num =
    Printf.printf "Creating game with %d players\n" player_num;
    let suits = [Spade; Heart; Club; Diamond] in
    let values = [Ace; Number 2; Number 3; Number 4; Number 5; Number 6;
                    Number 7; Number 8; Number 9; Number 10; Jack; Queen; King] in

    let deck = {
        cards = List.map (fun suit ->
                        List.map (fun value -> {value; suit}) values
                        ) suits
                    |> List.concat
                    |> Array.of_list;
        card_index = ref 0;
    } in


    Array.iter (fun card -> Printf.printf "%s " (string_of_card card)) deck.cards;
    Printf.printf "\n";

    let shuffled_deck = shuffle_cards deck in

    Printf.printf "After shuffling: \n";
    Array.iter (fun card -> Printf.printf "%s " (string_of_card card)) shuffled_deck.cards;
    Printf.printf "\n";

    let players = [
        {position = BTN; name = "Alice"; hole_cards = None; chips = 1000};
        {position = BB;  name = "Bob"; hole_cards = None; chips = 1000};
        {position = SB;  name = "Charles"; hole_cards = None; chips = 1000};
        {position = UTG; name = "David"; hole_cards = None; chips = 1000};
    ] in

    Printf.printf "Players before dealing:\n";

    List.iter (fun p -> Printf.printf "%s \n" (string_of_player p)) players;

    let players_with_cards = deal_cards_to_player deck players in

    Printf.printf "Players after dealing:\n";

    List.iter (fun p -> Printf.printf "%s \n" (string_of_player p)) players_with_cards
