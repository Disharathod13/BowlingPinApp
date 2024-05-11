#' Determine the winner of a bowling game between two players.
#'
#' This function calculates the scores for two players in a bowling game and determines the winner. It also displays the final scores of both players.
#'
#' @param player_rolls A numeric vector representing the scores for every throw of every player
#'
#' @return A character string indicating the winner of the game ("Player 1", "Player 2", or "Tie").
#'
#' @importFrom dplyr arrange %>%
#' @examples
#'
#' player_rolls <- player_throws(player_no = c(1, 2, 3),
#' throws = list(
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
#' c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
#' ))
#' bowling_winner(player_rolls)
#'
#' @export
bowling_winner <- function(player_rolls) {
  if (!inherits(player_rolls, "throws")) {
    stop("Parameter 'player_rolls' must be of class 'throws'. Please use player_throws function to create player_rolls")
  }
  scores <- calculate_bowling_score(player_rolls)
  scores <- scores |>
    arrange(-score)
  cat("Player",scores$player_no[1],"is the winner")
}


