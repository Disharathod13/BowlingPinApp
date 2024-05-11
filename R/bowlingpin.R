#' Calculate Bowling Pin Scores
#'
#' This function calculates the total scores for each player's throws in a bowling game and prints it in custom vector format.
#'
#'
#' @param player_rolls A record containing player numbers and their throws.
#' @return A record with players' scores calculated.
#' @export
#'
#' @examples
#' player_rolls <- player_throws(player_no = c(1, 2, 3),
#'   throws = list(
#'   c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
#'   c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
#'   c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
#'    ))
#'
#' bowlingpin(player_rolls)
#'
bowlingpin <- function(player_rolls) {
  players_df <- calculate_bowling_score(player_rolls)

  vec_cast.throws.data.frame((players_df))
}

#' Calculate the sum of throws
#'
#' This function calculates the total scores for each player's throws in a bowling game and returns a data frame.
#'
#'
#' @param player_rolls A record containing player numbers and their throws.
#' @return A record with players' scores calculated.
#'
#'@keywords internal
#'
calculate_bowling_score <- function(throws) {
  players_df <- vec_cast.data.frame.throws(throws)

  # Calculate sum of throws for each row and create a new column named "score"
  players_df$score <- sapply(players_df$throws, sum)

  return(players_df)
}


