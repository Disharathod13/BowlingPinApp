
# Main function to run the game for two players
#' Title
#'
#' @return
#' @export
#'
#' @examples
play_bowling_game <- function() {
  cat("Welcome to the ten-pin bowling game!\n")

  # Get throws for Player 1
  throws_player1 <- get_user_input(1)
  if (is.null(throws_player1)) {
    return()
  }

  # Get throws for Player 2
  throws_player2 <- get_user_input(2)
  if (is.null(throws_player2)) {
    return()
  }

  # Calculate scores for Player 1 and Player 2
  score_player1 <- calculate_bowling_score(throws_player1)
  score_player2 <- calculate_bowling_score(throws_player2)

  # Display scores
  cat("\nFinal scores:\n")
  cat("Player 1:", score_player1, "\n")
  cat("Player 2:", score_player2, "\n")
}


