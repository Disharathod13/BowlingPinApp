calculate_gutters <- function(player_rolls) {
  num_gutters <- sum(player_rolls == 0)
  return(num_gutters)
}

max_gutters <- function(player1_rolls, player2_rolls) {
  gutters_player1 <- calculate_gutters(player1_rolls)
  gutters_player2 <- calculate_gutters(player2_rolls)

  if (gutters_player1 > gutters_player2) {
    cat("Player 1 had the most gutters in the game with", gutters_player1, "gutters.")
  } else if (gutters_player2 > gutters_player1) {
    cat("Player 2 had the most gutters in the game with", gutters_player2, "gutters.")
  } else {
    cat("Both players had the same number of gutters in the game with", gutters_player1, "gutters each.")
  }
}

