### Generates possible outcomes ###

possible_outcomes <- t(combn(possible_hands, 5))
colnames(possible_outcomes) <- c('Card_1', 'Card_2', 'Card_3', 'Card_4', 'Card_5')

possible_outcomes %>% as_tibble()

# All detectors assume the player hand is composed by player_cards + cards_on_the_table

### Assesses possible outcomes ###

returns_kickers <- function(remaining_cards) {
  
  number_of_cards_remaining <- length(remaining_cards)
  number_of_cards_locked <- 7 - number_of_cards_remaining
  number_of_kickers_to_compute <- 5 - number_of_cards_locked
  
  if(number_of_kickers_to_compute > 0){
    card_ranking <- c('2','3','4','5','6','7','8','9','10','J','Q','K','A')
    kickers_boolean <- card_ranking %in% remaining_cards
    
    kicker_cards <- rev(card_ranking[kickers_boolean])[1:(number_of_kickers_to_compute-1)]
    kicker_rank <- rev(which(kickers_boolean == TRUE))[1:(number_of_kickers_to_compute-1)]
  }
  else {
    kicker_cards <- NULL
    kicker_rank <- NULL
  }
  
  kickers <- list(kicker_cards = kicker_cards, kicker_ranks = kicker_rank)
  
  return(kickers)
}

returns_kickers(c("A", "J", "2", "4"))

### Detects highest card ###

detects_high_card <- function(player_hand) {
  detected <- TRUE
  hand <- 'highest_card'
  hand_detected <- NULL
  suit <- 'several'
  global_rank <- 1
  hand_rank <- 0
  
  player_hand_no_suit <- str_extract(player_hand, '^[^-]*')
  card_ranking <- c('2','3','4','5','6','7','8','9','10','J','Q','K','A')
  
  participants_boolean <- card_ranking %in% player_hand_no_suit
  player_higher_cards <- card_ranking[participants_boolean][(length(player_hand_no_suit)-4):length(player_hand_no_suit)]
  hand_detected <- paste(rev(player_higher_cards), collapse = '-')
  hand_rank <- rev(which(participants_boolean == TRUE)[(length(player_hand_no_suit)-4):length(player_hand_no_suit)])
  outcome <- glue("Player has high cards in order: {paste(hand_detected, collapse = '-')}")
  
  high_card_detection <- list(detected = detected,
                          hand = hand,
                          hand_detected = hand_detected,
                          suit = suit,
                          global_rank = global_rank,
                          hand_rank = hand_rank,
                          outcome = outcome)
  return(high_card_detection)
  
}

detects_high_card(c('A', 'K', '10', '9', '8', '2'))

### Detects straights ###

detects_straights <- function(player_hand) {
  detected <- FALSE
  hand <- 'nothing'
  hand_detected <- NULL
  suit <- 'None'
  global_rank <- 0
  hand_rank <- 0
  outcome <- 'Player has no straight'
  
  player_hand_no_suit <- str_extract(player_hand, '^[^-]*')
  
  straight_generator_vector <- rev(c('A','K','Q','J','10','9','8','7','6','5','4','3','2','A'))
  possible_straights <- list()
  for (i in 1:10) {
      possible_straights[[i]] <- append(straight_generator_vector[i:(i+4)], i)
  }
  straight_matrix <- do.call(rbind, possible_straights)
  for (i in 1:nrow(straight_matrix)) {
    flag <- straight_matrix[i,1:5] %in% player_hand_no_suit
    if (all(flag)) {
      detected = TRUE
      hand <- 'straight'
      hand_detected <- straight_matrix[i,1:5]
      suit <- 'several'
      global_rank <- 4
      hand_rank <- as.integer(straight_matrix[i, 6])
      outcome <- glue("Player has straight: {paste(hand_detected, collapse = '-')}")
    }
  }

  straight_detection <- list(detected = detected,
                             hand = hand,
                             hand_detected = hand_detected,
                             global_rank = global_rank,
                             hand_rank = hand_rank,
                             outcome = outcome)
  return(straight_detection)
}

detects_straights(append(cards_dealt_to_table[[1]][[1]], cards_dealt_to_table[["cards_on_the_table"]]))

### Flush detector ###

cards_dealt_to_table

detects_flush <- function(player_hand) {
  detected <- FALSE
  hand <- 'nothing'
  hand_detected <- NULL
  suit <- 'None'
  global_rank <- 0
  hand_rank <- 0
  outcome <- 'Player has no flush'
  
  card_ranking <- c('2','3','4','5','6','7','8','9','10','J','Q','K','A')
  
  diamond_cards <- sum(str_count(player_hand, "Diamonds"))
  spade_cards <- sum(str_count(player_hand, "Spades"))
  club_cards <- sum(str_count(player_hand, "Clubs"))
  heart_cards <- sum(str_count(player_hand, "Hearts"))
  
  if (diamond_cards >= 5) {
    detected <- TRUE
    cards_involved <- str_c(player_hand[str_detect(player_hand, 'Diamonds')])
    suit <- "Diamonds"
  }
  else if (spade_cards >= 5) {
    detected <- TRUE
    cards_involved <- str_c(player_hand[str_detect(player_hand, 'Spades')])
    suit <- "Spades"
  }
  else if (club_cards >= 5) {
    detected <- TRUE
    cards_involved <- str_c(player_hand[str_detect(player_hand, 'Clubs')])
    suit <- "Clubs"
  }
  else if (heart_cards >= 5) {
    detected <- TRUE
    cards_involved <- str_c(player_hand[str_detect(player_hand, 'Hearts')])
    suit <- "Hearts"
  }
  
  if (detected) {
    hand <- 'flush'
    cards_involved_no_suit <- str_extract(cards_involved, '^[^-]*')
    flush_participants_boolean <- card_ranking %in% cards_involved_no_suit
    flush_cards <- card_ranking[flush_participants_boolean][(length(cards_involved_no_suit)-4):length(cards_involved_no_suit)]
    #flush_rank <- which(flush_participants_boolean == TRUE)[(length(cards_involved_no_suit)-4):length(cards_involved_no_suit)]
    hand_detected <- str_c(flush_cards, paste('-', suit, sep = ''))
    global_rank <- 5
    hand_rank <- rev(which(flush_participants_boolean == TRUE)[(length(cards_involved_no_suit)-4):length(cards_involved_no_suit)])
    outcome <- glue("Player has flush: {paste(hand_detected, collapse = ';')}")
  }
  
  flush_detection <- list(detected = detected,
                          hand = hand,
                          hand_detected = hand_detected,
                          suit = suit,
                          global_rank = global_rank,
                          hand_rank = hand_rank,
                          outcome = outcome)
  return(flush_detection)
}

detects_flush(c("A-Diamonds", "K-Diamonds", "Q-Diamonds", "J-Diamonds", "10-Diamonds"))

### Pair, trio, quad and Full House detector ###

cards_dealt_to_table

detects_pair_trio_quad_fullHouse <- function(player_hand) {
  detected <- FALSE
  hand <- 'nothing'
  hand_detected <- NULL
  suit <- 'None'
  global_rank <- 0
  hand_rank <- 0
  outcome <- 'Player has no pair, trio, quad or Full House'
  
  
  player_hand_no_suit <- str_extract(player_hand, '^[^-]*')
  card_ranking <- c('2','3','4','5','6','7','8','9','10','J','Q','K','A')
  card_count <- c()
  
  for (i in card_ranking){
    card_count[i] <- sum(str_count(player_hand_no_suit, i))
  }
  
  if (any(card_count == 2)){
    detected <- TRUE
    hand_detected <- names(card_count[card_count==2])
    if (length(hand_detected) > 1) {
      hand <- 'two-pair'
      global_rank <- 3
      outcome <- glue("Player has pair(s): {paste(hand_detected, collapse = '-')}")
    }
    else if (length(hand_detected) == 1){
      hand <- 'pair'
      global_rank <- 2
      outcome <- glue("Player has pair: {paste(hand_detected, collapse = '-')}")
    }
    suit <- 'several'
    hand_rank <- rev(which(card_count==2))
  }
  if (any(card_count == 3)){
    detected <- TRUE
    hand <- 'trio'
    hand_detected <- names(card_count[card_count==3])
    suit <- 'several'
    global_rank <- 3
    hand_rank <- which(card_count==3)
    #cards <- names(card_count[card_count==3])
    #cards_rank <- which(card_count==3)
    outcome <- glue("Player has trio(s): {paste(hand_detected, collapse = '-')}")
  }
  if (any(card_count == 4)){
    detected <- TRUE
    hand <- 'quad'
    hand_detected <- names(card_count[card_count==4])
    suit <- 'several'
    global_rank <- 6
    hand_rank <- which(card_count==4) 
    #cards <- names(card_count[card_count==4])
    #cards_rank <- which(card_count==4)
    outcome <- glue("Player has quad: {paste(hand_detected, collapse = '-')}")
  }
  if (any(card_count == 2) & any(card_count == 3)){
    detected <- TRUE
    hand <- 'fullHouse'
    pair_rank <- which(card_count==2)
    trio_rank <- which(card_count==3)
    #cards_rank <- append(trio_rank, pair_rank)
    pair_card <- names(card_count[card_count==2])
    trio_card <- names(card_count[card_count==3])
    #cards <- append(trio_card, pair_card)
    hand_detected <- append(trio_card, pair_card)
    suit <- 'several'
    global_rank <- 5
    hand_rank <- append(trio_rank, pair_rank)
    
    outcome <- glue("Player has full house: {trio_card}'s over {pair_card}'s")
  }
  
  remaining_cards <- player_hand_no_suit[!player_hand_no_suit %in% hand_detected]
  kickers <- returns_kickers(remaining_cards = remaining_cards)
  
  pair_trio_quad_fullHouse_detection <- list(detected = detected,
                                                     hand = hand,
                                                     hand_detected = hand_detected,
                                                     suit = suit,
                                                     global_rank = global_rank,
                                                     hand_rank = hand_rank,
                                                     kickers = kickers,
                                                     outcome = outcome)
  
  return(pair_trio_quad_fullHouse_detection)
}

detects_pair_trio_quad_fullHouse(c('A', '2', 'J', 'J', '4', '8', '9'))

### Detects straight_flush, Royal Flush ###


detects_straight_and_royal_flush <- function(player_hand) {
  detected <- FALSE
  straight_flush <- 'None'
  royal_flush <- 'None'
  outcome <- 'Player has no straight flush or royal flush'
  
  detected <- FALSE
  hand <- 'nothing'
  hand_detected <- NULL
  suit <- 'None'
  global_rank <- 0
  hand_rank <- 0
  outcome <- 'Player has no straight flush or Royal Flush'
  
  straight_detector <- detects_straights(player_hand = player_hand)
  flush_detector <- detects_flush(player_hand = player_hand)
  suit <- flush_detector[["suit"]]
  
  if ( (straight_detector[["detected"]] & straight_detector[["hand_rank"]] == 10) & flush_detector[["detected"]]) {
    detected <- TRUE
    hand <- 'Royal Flush'
    straight_cards <- straight_detector[["hand_detected"]]
    hand_detected <- str_c(straight_cards, paste('-', suit, sep = ''))
    outcome <- glue("Player has Royal Flush: {paste(hand_detected,collapse=';')}")
  }
  if ( (straight_detector[["detected"]] & straight_detector[["hand_rank"]] != 10) & flush_detector[["detected"]]) {
    detected <- TRUE
    hand <- 'Straight Flush'
    straight_cards <- straight_detector[["hand_detected"]]
    hand_detected <- str_c(straight_cards, paste('-', suit, sep = ''))
    outcome <- glue("Player has Straight Flush: {paste(hand_detected,collapse=';')}")
  }
  
  global_rank <- 7
  hand_rank <- straight_detector[["hand_rank"]]
  
  straight_and_royal_flush_detection <- list(detected = detected,
                                             hand = hand,
                                             hand_detected = hand_detected,
                                             suit = suit,
                                             global_rank = global_rank,
                                             hand_rank = hand_rank,
                                             outcome = outcome)
  
  return(straight_and_royal_flush_detection)
}

detects_straight_and_royal_flush(c("9-Clubs", 'K-Diamonds', 'Q-Diamonds', 'J-Diamonds', '10-Diamonds', 'J-Hearts'))

detects_pair_trio_quad_fullHouse(c("9-Clubs", 'K-Diamonds', 'Q-Diamonds', 'J-Diamonds', '10-Diamonds', 'J-Hearts'))

### Computes hand ###

computes_hands <- function(cards_dealt_to_table) {
  player_hands <- list()
  
  for (i in 1:length(cards_dealt_to_table[["player_cards"]])){
    
    player_cards <- cards_dealt_to_table[["player_cards"]][[i]]
    player_cards_with_table <- append(player_cards, cards_dealt_to_table[["cards_on_the_table"]])
    
    pair_trio_quad_fullHouse <- detects_pair_trio_quad_fullHouse(player_hand = player_cards_with_table)
    straight <- detects_straights(player_hand = player_cards_with_table)
    flush <- detects_flush(player_hand = player_cards_with_table)
    straight_royal_flush <- detects_straight_and_royal_flush(player_hand = player_cards_with_table)
    
    if (pair_trio_quad_fullHouse[["detected"]]) {
      outcome <- pair_trio_quad_fullHouse[["outcome"]]
      global_rank <- pair_trio_quad_fullHouse[["global_rank"]]
      hand_rank <- pair_trio_quad_fullHouse[["hand_rank"]]
    }
    else if (straight[["detected"]]) {
      outcome <- straight[["outcome"]]
      global_rank <- straight[["global_rank"]]
      hand_rank <- straight[["hand_rank"]]
    }
    else if (flush[["detected"]]) {
      outcome <- flush[["outcome"]]
      global_rank <- flush[["global_rank"]]
      hand_rank <- flush[["hand_rank"]]
    }
    else if (straight_royal_flush[["detected"]]) {
      outcome <- straight_royal_flush[["outcome"]]
      global_rank <- straight_royal_flush[["global_rank"]]
      hand_rank <- straight_royal_flush[["hand_rank"]]
    }
    else {
      high_card <- detects_high_card(player_hand = player_cards_with_table)
      outcome <- high_card[["outcome"]]
      global_rank <- high_card[["global_rank"]]
      hand_rank <- high_card[["hand_rank"]]
    }
    
    player_hands[[paste("player",i,sep = '-')]] <- list(outcome = outcome, global_rank = global_rank, hand_rank = hand_rank) 
  }
  
  cards_dealt_to_table[["player_hands"]] <- player_hands 
  
  return(cards_dealt_to_table)
  
}


computes_hands(cards_dealt_to_table = cards_dealt_to_table)
