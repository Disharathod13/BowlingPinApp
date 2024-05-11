#' Player Throws and Scores
#'
#' Functions to handle player throws in a ten pin bowling game and calculate their scores.
#'
#' @param player_no Player number
#' @param throws A list of throws of each player
#' @param score Total score
#'
#' @return Throws a record
#' @export
#'
#' @examples
#'
#' player_rolls <- player_throws(player_no = c(1, 2, 3),
#' throws = list(
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
#' c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
#' c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
#' ))
player_throws <- function(player_no, throws, score= numeric()) {
  if (length(player_no) != length(throws)) {
    stop("Length of player_no and throws must be the same.")
  }

  # Check if player_no is numeric
  vctrs::vec_assert(player_no, numeric())

  # Check if player_no is numeric
  vctrs::vec_assert(score, numeric())
  if(length(score) == 0){
    score <- numeric(length(player_no))
  }
  # Check if throws is a list
  vctrs::vec_assert(throws, list())

  # Check if throws is a list of numeric vectors
  if (!all(sapply(throws, function(x)
    vctrs::vec_is(x, numeric())))) {
    stop("throws must be a list of numeric vectors.")
  }

  # Check if each player's throws follow bowling game rules
  for (i in seq_along(throws)) {
    player_throws <- throws[[i]]

    # Check if throws is not more than 10
    if (any(throws[[i]] > 10)) {
      stop("10 is the max points for any throw")
    }
    frame <- 1
    throw_index <- 1

    while (frame <= 10) {
      # Check for the 10th frame
      if (frame == 10) {
        frame_pins <- sum(player_throws[throw_index:(throw_index + 1)])
        if (frame_pins > 10 && player_throws[throw_index] != 10) {
          stop(
            "For 10th frame: Sum of first two throws cannot exceed 10 if first throw isn't a strike"
          )
        }
        if (frame_pins < 10 && length(player_throws) == 21) {
          stop(
            "Only 20 throws are allowed for player ",
            i,
            " since they didn't get a strike or spare in first 2 throws in 10th frame"
          )
        }
        else if (frame_pins == 10 && length(player_throws) == 20) {
          stop("Player ",
               i,
               " gets an additional throw since they got a spare in 10th frame")
        } else if (player_throws[throw_index] == 10) {
          if (length(player_throws) == 20) {
            stop(
              "Player ",
              i,
              " gets two additional throws since they got a strike in 10th frame's first throw"
            )
          }
          throw_index <- throw_index + 1
          frame_pins <-
            sum(player_throws[throw_index:(throw_index + 1)])
          if (player_throws[throw_index] == 10) {
            throw_index <- throw_index + 2
          } else if (frame_pins > 10) {
            stop("No 2nd strike in 10th frame, so max score possible in last 2 throws is 10.")
          }
        }
      }
      else{
        # For frames 1 to 9
        # Check if the frame score doesnt exceed 10
        frame_pins <-
          sum(player_throws[throw_index:(throw_index + 1)])
        if (frame_pins < 0 || frame_pins > 10) {
          stop("Invalid input! Number of pins knocked down in a frame must be between 0 and 10.")
        }
        # Check if the first throw is a strike. In that case, skip a throw in that frame.
        if (player_throws[throw_index] == 10) {
          throw_index <- throw_index + 1
        } else {
          throw_index <- throw_index + 2
        }
      }
      frame <- frame + 1
    }

    if (frame <= 10) {
      stop("Invalid input! Not all frames have been completed.")
    }
  }

  throws <- vctrs::new_rcrd(list(player_no = player_no,
                       throws = throws,
                       score = score),
                  class = "throws")
  return(throws)
}

# Casting throws to data frame
vec_cast.data.frame.throws <- function(x, to, ...){
  vctrs::vec_data(x)
}

# Casting data frame to throws
vec_cast.throws.data.frame <- function(x, to, ...){
  player_throws(x$player_no,x$throws, x$score)
}


#' @export
# Redefine the custom format method for the throws class
format.throws <- function(x, ...) {
  players <- vctrs::field(x, "player_no")
  throws <- vctrs::field(x, "throws")
  score <- vctrs::field(x,"score")
  n_players <- length(players)

  scorecard <- matrix("", nrow = 21, ncol = n_players)
  colnames(scorecard) <- paste("Player", players)
  rownames(scorecard) <- c(
    "Throw 1", "Throw 2", "Throw 3", "Throw 4", "Throw 5", "Throw 6",
    "Throw 7", "Throw 8", "Throw 9", "Throw 10", "Throw 11", "Throw 12",
    "Throw 13", "Throw 14", "Throw 15", "Throw 16", "Throw 17", "Throw 18",
    "Throw 19", "Throw 20", "Throw 21"
  )

  for (i in seq_len(n_players)) {
    player_throws <- throws[[i]]
    frame <- 1
    throw_index <- 1

    for (frame in 1:10) {
      if (frame == 10) {
        scorecard[2 * frame - 1, i] <- player_throws[throw_index]

        if (player_throws[throw_index] == 10) {
          scorecard[2 * frame - 1, i] <- "X"

          if (player_throws[throw_index + 1] == 10) {
            scorecard[2 * frame, i] <- "X"
            scorecard[2 * frame + 1, i] <- ifelse(player_throws[throw_index + 2] == 10, "X", player_throws[throw_index + 2])
          } else {
            scorecard[2 * frame, i] <- ifelse(player_throws[throw_index + 1] == 0, "-", player_throws[throw_index + 1])
            scorecard[2 * frame + 1, i] <- ifelse(player_throws[throw_index + 2] == 10, "X", player_throws[throw_index + 2])
          }

          if (length(player_throws) == 21 && player_throws[21] == 10) {
            scorecard[2 * frame + 1, i] <- "X"
          }
        } else {
          if (player_throws[throw_index] + player_throws[throw_index + 1] == 10) {
            scorecard[2 * frame - 1, i] <- player_throws[throw_index]
            scorecard[2 * frame, i] <- "/"
          } else {
            scorecard[2 * frame - 1, i] <- player_throws[throw_index]
            scorecard[2 * frame, i] <- ifelse(player_throws[throw_index + 1] == 0, "-", player_throws[throw_index + 1])
          }
          scorecard[2 * frame + 1, i] <- ifelse(length(player_throws) == 21, player_throws[21], "-")
        }
      } else {
        if (player_throws[throw_index] == 10) {
          scorecard[2 * frame - 1, i] <- "X"
          scorecard[2 * frame, i] <- "-"
          throw_index <- throw_index + 1
        } else {
          scorecard[2 * frame - 1, i] <- player_throws[throw_index]

          if (player_throws[throw_index] + player_throws[throw_index + 1] == 10) {
            scorecard[2 * frame, i] <- "/"
          } else {
            scorecard[2 * frame, i] <- ifelse(player_throws[throw_index + 1] == 0, "-", player_throws[throw_index + 1])
          }
          throw_index <- throw_index + 2
        }
      }
    }
  }

  scorecard <- apply(scorecard, 2, as.character)

  output <- character(nrow(scorecard))
  for (i in seq_len(nrow(scorecard))) {
    output[i] <- paste(scorecard[i, ], collapse = "         ")
  }
  for (i in seq_len(ncol(scorecard))){
    cat("Player",i, " ")
  }
  cat("\n")
  cat(output, sep = "\n")
  suppressWarnings({
  if(any(score) != 0){
    for (i in seq_len(ncol(scorecard))){
      cat("---------" )
    }
    cat("\n")
    cat(score, sep = "        ")
    cat("\n")
  }
  })
}

