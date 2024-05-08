#' Calculate the number of strikes in a player's rolls.
#'
#' This function counts the number of strikes (rolls with a score of 10) in a player's rolls.
#'
#' @param player_rolls A numeric vector representing the scores for every throw of a player.
#'
#' @return An integer indicating the number of strikes in the player's rolls.
#'
#' @keywords internal
#'
#'
calculate_strikes <- function(player_rolls) {
  num_strikes <- sum(player_rolls == 10)
  return(num_strikes)
}


#' Determine the player with the most strikes in the game.
#'
#' This function compares the number of strikes between two players and identifies the player with the highest number of strikes.
#'
#' @param player1_rolls A numeric vector representing the scores for every throw of player 1.
#' @param player2_rolls A numeric vector representing the scores for every throw of player 2.
#'
#' @return NULL (prints the player with the most strikes and their corresponding number of strikes).
#'
#' @examples
#' player1_rolls <- c(10, 3, 4, 10, 5, 10, 7, 2, 3, 4, 10, 10, 10, 6, 7, 8, 9, 1, 2, 3)
#' player2_rolls <- c(10, 10, 4, 3, 5, 10, 10, 2, 3, 4, 10, 10, 10, 6, 7, 8, 9, 1, 2, 3)
#' max_strikes(player1_rolls, player2_rolls)
#'
#' @export
max_strikes <- function(player1_rolls, player2_rolls) {
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

