start <- Sys.time()
shuffled_deck <- shuffles_deck(possible_hands = possible_hands)

cards_dealt_to_players <- deals_cards_to_players(shuffled_deck = shuffled_deck, number_of_players = 6, number_of_cards_per_player = 2)

cards_dealt_to_table <- deals_cards_to_table(cards_dealt_to_players = cards_dealt_to_players)

computes_hands(cards_dealt_to_table = cards_dealt_to_table)
end <- Sys.time()

end - start