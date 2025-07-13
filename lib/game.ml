 let deal_community_cards deck =
     let deal_top_5_card = Cards.deal_top_n_card 5 in
     match deal_top_5_card deck with
     | Some(cards, new_deck) -> (cards, new_deck)
     | None -> failwith "Not enough cards in deck for community cards"

let create =
    let deck = Cards.create_ordered_deck |> Cards.shuffle in
    let players = Players.create_players () in
    let (players_with_cards, deck) = Players.deal_cards_to_player deck players in
    let (community_cards, _) = deal_community_cards deck in
    List.iter (fun p -> Printf.printf "%s \n" (Players.string_of_player p)) players_with_cards;
    Array.iter (fun card -> Printf.printf "%s " (Cards.string_of_card card)) community_cards;
