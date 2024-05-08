calculate_strikes <- function(player_rolls) {
  num_strikes <- sum(player_rolls == 10)
  return(num_strikes)
}

player_with_most_strikes <- function(player1_rolls, player2_rolls) {
  strikes_player1 <- calculate_strikes(player1_rolls)
  strikes_player2 <- calculate_strikes(player2_rolls)

  if (strikes_player1 > strikes_player2) {
    cat("Player 1 had the most strikes in the game with", strikes_player1, "strikes.")
  } else if (strikes_player2 > strikes_player1) {
    cat("Player 2 had the most strikes in the game with", strikes_player2, "strikes.")
  } else {
    cat("Both players had the same number of strikes in the game with", strikes_player1, "strikes each.")
  }
}

