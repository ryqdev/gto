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
    (string_of_card_value card.value) ^ (string_of_card_suit card.suit)

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
    Printf.printf "\n"
