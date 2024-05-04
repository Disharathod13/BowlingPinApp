#' Main function to run the ten-pin bowling game for two players
#' @description
#' Enter your individual game scores and the function will give you the total score at the end
#'
#' @param player1_rolls A numeric vector having scores for every throw of player 1
#' @param player2_rolls A numeric vector having scores for every throw of player 2
#'
#' @return A numeric vector containing the total score for each user.
#' @export
#'
#' @examples
#' player1_rolls <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
#' player2_rolls <- c(2,2,2,2,8,2,6,2,2,2,2,2,5,2,2,2,4,2,2,3)
#' bowlingpin(player1_rolls,player2_rolls)
bowlingpin <- function(player1_rolls,player2_rolls) {
  cat("Welcome to the ten-pin bowling game!\n")

  # Get throws for Player 1
  if (is.null(player1_rolls)) {
    return()
  }

  # Get throws for Player 2
  if (is.null(player2_rolls)) {
    return()
  }

  calculate_bowling_score <- function(throws) {
    score <- 0
    throw_index <- 1
    frame <- 1

    while (frame <= 10) {
      if (frame == 10) {
        # Check for the 10th frame
        if (throws[throw_index] == 10) {  # Strike on first throw
          score <- score + 10 + throws[throw_index + 1] + throws[throw_index + 2]
          throw_index <- throw_index + 1
        } else if (sum(throws[throw_index:(throw_index + 1)]) == 10) {  # Spare on first two throws
          score <- score + 10 + throws[throw_index + 2]
          throw_index <- throw_index + 2
        } else {  # Open frame
          score <- score + throws[throw_index] + throws[throw_index + 1]
          throw_index <- throw_index + 2
        }
      } else {
        if (throws[throw_index] == 10) {  # Strike
          score <- score + 10 + throws[throw_index + 1] + throws[throw_index + 2]
          throw_index <- throw_index + 1
        } else if (sum(throws[throw_index:(throw_index + 1)]) == 10) {  # Spare
          score <- score + 10 + throws[throw_index + 2]
          throw_index <- throw_index + 2
        } else {  # Open frame
          score <- score + throws[throw_index] + throws[throw_index + 1]
          throw_index <- throw_index + 2
        }
      }

      frame <- frame + 1
    }

    return(score)
  }

  # Calculate scores for Player 1 and Player 2
  score_player1 <- calculate_bowling_score(player1_rolls)
  score_player2 <- calculate_bowling_score(player2_rolls)

  # Display scores
  cat("\nFinal scores:\n")
  cat("Player 1:", score_player1, "\n")
  cat("Player 2:", score_player2, "\n")
  invisible(c(score_player1,score_player2))
}


