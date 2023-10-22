library(tidyr)

available_figures <- c('A','K','Q','J','10','9','8','7','6','5','4','3','2')
available_suits <- c('Spades', 'Hearts', 'Clubs', 'Diamonds')

possible_hands <- c()
for (i in 1:length(available_figures)){
  for (k in 1:length(available_suits)){
    hand <- paste(available_figures[i], '-', available_suits[k], sep = '')
    possible_hands <- append(possible_hands, hand)
  }
}

shuffles_deck <- function(possible_hands) {
  shuffled_deck <- sample(possible_hands)
  
  return(shuffled_deck)
}

shuffled_deck <- shuffles_deck(possible_hands = possible_hands)

deals_cards_to_players <- function(shuffled_deck, number_of_players, number_of_cards_per_player) {
  
  player_card_vector <- rep(1:number_of_players, number_of_cards_per_player)
  cards_to_deal <- sample(shuffled_deck, number_of_players * number_of_cards_per_player)
  
  player_numbers <- c()
  for (i in 1:number_of_players) {player_numbers <- append(player_numbers, paste('player-', i, sep = ''))}
  
  player_cards <- vector(mode = "list", length = length(player_numbers))
  names(player_cards) <- player_numbers
  
  cards_to_deal_to_players <- sample(shuffled_deck, number_of_players * number_of_cards_per_player)
  for (i in 1:number_of_players) {player_cards[[i]] <- cards_to_deal_to_players[which(player_card_vector == i)]}
  
  cards_after_dealing_to_players <- shuffled_deck[!shuffled_deck %in% cards_to_deal_to_players]
  
  cards_dealt_to_players <- list(player_cards = player_cards, cards_after_dealing_to_players = cards_after_dealing_to_players)
  
  return (cards_dealt_to_players)
}

cards_dealt_to_players <- deals_cards_to_players(shuffled_deck = shuffled_deck, number_of_players = 2, number_of_cards_per_player = 2)

deals_cards_to_table <- function(cards_dealt_to_players) {
  
  table_card_vector <- c('burn', rep('flop', 3), 'burn', 'turn', 'burn', 'river')
  
  cards_after_dealing_to_players <- cards_dealt_to_players[["cards_after_dealing_to_players"]]
  
  cards_to_deal_to_table <- cards_after_dealing_to_players[1:length(table_card_vector)]
  
  burned_cards <- cards_to_deal_to_table[which(table_card_vector == 'burn')]
  cards_on_the_table <- cards_to_deal_to_table[which(table_card_vector != 'burn')]
  flop <- cards_to_deal_to_table[which(table_card_vector == 'flop')]
  turn <- cards_to_deal_to_table[which(table_card_vector == 'turn')]
  river <- cards_to_deal_to_table[which(table_card_vector == 'river')]
  
  cards_dealt_to_table <- list(player_cards = cards_dealt_to_players[["player_cards"]],
                               cards_after_dealing_to_players = cards_dealt_to_players[["cards_after_dealing_to_players"]],
                               cards_to_deal_to_table = cards_to_deal_to_table,
                               burned_cards = burned_cards,
                               cards_on_the_table = cards_on_the_table,
                               flop = flop,
                               turn = turn,
                               river = river)
  
  return (cards_dealt_to_table)
  
}

cards_dealt_to_table <- deals_cards_to_table(cards_dealt_to_players = cards_dealt_to_players)

