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

(* let string_of_card_suit = function *)
(*    | Spade -> "♠" *)
(*    | Heart -> "♥" *)
(*    | Diamond -> "♦" *)
(*    | Club -> "♣" *)

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

let shuffle_cards cards = cards

let create player_num =
    Printf.printf "Creating game with %d players\n" player_num;
    let suits = [Spade; Heart; Club; Diamond] in
    let values = [Ace; Number 2; Number 3; Number 4; Number 5; Number 6;
                    Number 7; Number 8; Number 9; Number 10; Jack; Queen; King] in
    let cards = List.map (fun suit ->
        List.map (fun value -> {value; suit}) values
        ) suits
    |> List.concat
    |> Array.of_list in

    Array.iter (fun card -> Printf.printf "%s " (string_of_card card)) cards;
    Printf.printf "\n";

    let cards = shuffle_cards cards in

    Printf.printf "After shuffling: \n";
    Array.iter (fun card -> Printf.printf "%s " (string_of_card card)) cards;
    Printf.printf "\n";

    let players = [
        {position = BTN; name = "A"; hole_cards = None; chips = 1000};
        {position = BB;  name = "B"; hole_cards = None; chips = 1000};
        {position = SB;  name = "C"; hole_cards = None; chips = 1000};
        {position = UTG; name = "D"; hole_cards = None; chips = 1000};
    ] in

    List.iter (fun p -> Printf.printf "%s \n" (string_of_player p)) players

    Printf.printf "Game start!\n"

