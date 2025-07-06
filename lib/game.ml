 let deal_community_cards deck =
     Printf.printf "Dealing community cards...\n";
     let deal_top_5_card = Cards.deal_top_n_card 5 in
     match deal_top_5_card deck with
     | Some(cards, new_deck) -> (cards, new_deck)
     | None -> failwith "Not enough cards in deck for community cards"

let create =
    Printf.printf "\n===Creating game===\n";
    let deck = Cards.shuffle Cards.create_ordered_deck in
    Array.iter (fun card -> Printf.printf "%s " (Cards.string_of_card card)) deck.cards;
    Printf.printf "\n";

    let players = Players.create_players () in

    Printf.printf "\n===Players before dealing===\n";
    List.iter (fun p -> Printf.printf "%s \n" (Players.string_of_player p)) players;

    let (players_with_cards, deck) = Players.deal_cards_to_player deck players in

    Printf.printf "\n===Players after dealing===\n";
    List.iter (fun p -> Printf.printf "%s \n" (Players.string_of_player p)) players_with_cards;

    let (community_cards, _) = deal_community_cards deck in
    Array.iter (fun card -> Printf.printf "%s " (Cards.string_of_card card)) community_cards;
