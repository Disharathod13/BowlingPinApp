bowling_leaderboard <- function(player1_rolls, player2_rolls) {
  if (is.null(player1_rolls) || is.null(player2_rolls)) {
    stop("Both players must have rolls.")
  }

  # Calculate scores for Player 1 and Player 2
  score_player1 <- calculate_bowling_score(player1_rolls)
  score_player2 <- calculate_bowling_score(player2_rolls)

  # Display scores
  cat("Final scores:\n")
  cat("Player 1:", score_player1, "\n")
  cat("Player 2:", score_player2, "\n")

  # Determine the winner
  if (score_player1 > score_player2) {
    cat("Player 1 wins!")
    winner <- "Player 1"
  } else if (score_player2 > score_player1) {
    cat("Player 2 wins!")
    winner <- "Player 2"
  } else {
    cat("It's a tie!")
    winner <- "Tie"
  }

  return(winner)
}


