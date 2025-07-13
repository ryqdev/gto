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
   | BTN -> "BTN"
   | SB -> "SB"
   | BB -> "BB"
   | UTG -> "UTG"

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
    let positions = [BTN; SB; BB; UTG] in
    List.mapi (fun i position ->
        {position; name = string_of_int i; hole_cards = None; chips = 1000}
    ) positions

let deal_cards_to_player deck players =
    let player_num = List.length players in
    let cards_to_deal = Cards.deal_top_n_card (player_num * 2) in
    match cards_to_deal deck with
    | Some (cards, new_deck) ->
        let updated_players =
            List.mapi (fun i player ->
                {player with hole_cards = Some(cards.(i), cards.(i + player_num))}
            ) players in
        (updated_players, new_deck)
    | None -> failwith "Not enough cards in deck"
