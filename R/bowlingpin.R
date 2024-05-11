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
#' player_rolls<- player_throws(player_no = c(1, 2, 3),
#' throws = list(
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
#' c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
#' ))
#' bowlingpin(player_rolls)

bowlingpin <- function(player_rolls) {
  players_df <- vec_cast.data.frame.throws(player_rolls)

  # Calculate sum of throws for each row and create a new column named "score"
  players_df$score <- sapply(players_df$throws, sum)

  vec_cast.throws.data.frame((players_df))
}

calculate_bowling_score <- function(throws) {
  players_df <- vec_cast.data.frame.throws(throws)

  # Calculate sum of throws for each row and create a new column named "score"
  players_df$score <- sapply(players_df$throws, sum)

  return(players_df)
}

# unvectorised bowling score with validations
# calculate_bowling_score <- function(throws) {
#   score <- 0
#   throw_index <- 1
#   frame <- 1
#
#   while (frame <= 10) {
#     if (frame == 10) {
#       # Check for the 10th frame
#       if (throws[throw_index] == 10) {
#         # Strike on first throw
#         score <-
#           score + 10 + throws[throw_index + 1] + throws[throw_index + 2]
#         throw_index <- throw_index + 1
#       } else if (sum(throws[throw_index:(throw_index + 1)]) == 10) {
#         # Spare on first two throws
#         score <- score + 10 + throws[throw_index + 2]
#         throw_index <- throw_index + 2
#       } else {
#         # Open frame
#         score <-
#           score + throws[throw_index] + throws[throw_index + 1]
#         throw_index <- throw_index + 2
#       }
#     } else {
#       if (throws[throw_index] == 10) {
#         # Strike
#         score <-
#           score + 10 + throws[throw_index + 1] + throws[throw_index + 2]
#         throw_index <- throw_index + 1
#       } else if (sum(throws[throw_index:(throw_index + 1)]) == 10) {
#         # Spare
#         score <- score + 10 + throws[throw_index + 2]
#         throw_index <- throw_index + 2
#       } else {
#         # Open frame
#         score <-
#           score + throws[throw_index] + throws[throw_index + 1]
#         throw_index <- throw_index + 2
#       }
#     }
#
#     frame <- frame + 1
#   }
#
#   return(score)
# }
