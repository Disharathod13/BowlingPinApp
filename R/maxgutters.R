#' Calculate the number of gutters in a player's rolls.
#'
#' @param player_rolls A numeric vector representing the scores for every throw of a player.
#'
#' @return An integer representing the number of gutters in the player's rolls.
#'
#' @keywords internal
#'
calculate_gutters <- function(player_rolls) {
  num_gutters <- sum(player_rolls == 0)
  return(num_gutters)
}

#' Determine the player with the most gutters in a game.
#'
#' This function compares the number of gutters for two players and determines which player had the most gutters in the game.
#'
#' @param player1_rolls A numeric vector representing the scores for every throw of player 1.
#' @param player2_rolls A numeric vector representing the scores for every throw of player 2.
#'
#' @return A message indicating which player had the most gutters and the number of gutters they had.
#' @examples
#' player1_rolls <- c(0, 0, 3, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#' player2_rolls <- c(0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0)
#' max_gutters(player1_rolls, player2_rolls)
#'
#' @export
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

