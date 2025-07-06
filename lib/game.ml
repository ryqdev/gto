 let deal_community_cards deck =
     Printf.printf "Dealing community cards...\n";
     let community_cards = ref [] in

     for _ = 1 to 5 do
        match Cards.deal_top_card deck with
        | Some (card) ->
            community_cards := card :: !community_cards;
        | None -> failwith "Not enough cards in deck for community cards"
     done;

     Array.of_list (List.rev !community_cards)

let create () =
    Printf.printf "\n===Creating game===\n";
    let deck = Cards.create_ordered_deck () in
    Array.iter (fun card -> Printf.printf "%s " (Cards.string_of_card card)) deck.cards;
    Printf.printf "\n";

    let players = Players.create_players () in

    Printf.printf "\n===Players before dealing===\n";
    List.iter (fun p -> Printf.printf "%s \n" (Players.string_of_player p)) players;

    let players_with_cards = Players.deal_cards_to_player deck players in

    Printf.printf "\n===Players after dealing===\n";
    List.iter (fun p -> Printf.printf "%s \n" (Players.string_of_player p)) players_with_cards;

    let community_cards = deal_community_cards deck in
    Array.iter (fun card -> Printf.printf "%s " (Cards.string_of_card card)) community_cards;
