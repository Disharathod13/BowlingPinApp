
# Main function to run the game for two players
#'
#' @return A numeric vector containing the total score for each user.
#' @export
#'
#' @examples
#' bowlingpin()
bowlingpin <- function() {
  cat("Welcome to the ten-pin bowling game!\n")

  # Get throws for Player 1
 player1_rolls <- rollinput(1)
  if (is.null(player1_rolls)) {
    return()
  }

  # Get throws for Player 2
  player2_rolls <- rollinput(2)
  if (is.null(player2_rolls)) {
    return()
  }

  # Calculate scores for Player 1 and Player 2
  score_player1 <- calculate_bowling_score(player1_rolls)
  score_player2 <- calculate_bowling_score(player2_rolls)

  # Display scores
  cat("\nFinal scores:\n")
  cat("Player 1:", score_player1, "\n")
  cat("Player 2:", score_player2, "\n")
}


