type player_position =
    | BTN
    | SB
    | BB
    | UTG

type player =
    { position: player_position
    ; name: string
    ; hole_cards: (Cards.card * Cards.card) option
    ; chips: int
    }


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
                (Cards.string_of_card card1)
                (Cards.string_of_card card2)
    in
    Printf.sprintf "%s %s %s %s"
        (string_of_player_position player.position)
        player.name
        card_str
        (string_of_int player.chips)



let create_players () =
        [
            {position = BTN; name = "Alice"; hole_cards = None; chips = 1000};
            {position = BB;  name = "Bob"; hole_cards = None; chips = 1000};
            {position = SB;  name = "Charles"; hole_cards = None; chips = 1000};
            {position = UTG; name = "David"; hole_cards = None; chips = 1000};
        ]

let deal_cards_to_player deck players =
    let deal_round players round_count =
        Printf.printf "Dealing cards in round %d" round_count;
        List.map (fun p ->
            match Cards.deal_top_card deck with
            | Some (card) ->
                Printf.printf "%s gets %s\n" p.name (Cards.string_of_card card);
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
