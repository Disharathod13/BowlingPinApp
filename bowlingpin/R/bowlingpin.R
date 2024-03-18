# Function to take user input for throws for a single player
get_user_input <- function(player_num) {
  cat("Player", player_num, ":\n")
  throws <- numeric(0)
  frame <- 1

  while (frame <= 10) {
    if (frame == 10) {
      # Check for the 10th frame
      pins <- as.integer(readline(prompt = "Enter number of pins knocked down in throw 1: "))
      if (pins < 0 || pins > 10) {
        cat("Invalid input! Number of pins knocked down must be between 0 and 10.\n")
        return(NULL)
      }
      throws <- c(throws, pins)

      if (pins == 10) {
        # If strike, allow two more throws
        pins <- as.integer(readline(prompt = "Enter number of pins knocked down in throw 2: "))
        if (pins < 0 || pins > 10) {
          cat("Invalid input! Number of pins knocked down must be between 0 and 10.\n")
          return(NULL)
        }
        throws <- c(throws, pins)

        if (throws[length(throws)-1] + throws[length(throws)] < 10) {
          # If first two throws don't result in a spare, break the loop
          break
        }
      } else {
        # If not strike, check for spare
        pins <- as.integer(readline(prompt = "Enter number of pins knocked down in throw 2: "))
        if (pins < 0 || pins > (10 - throws[length(throws)])) {
          cat("Invalid input! Number of pins knocked down must be between 0 and", (10 - throws[length(throws)]), ".\n")
          return(NULL)
        }
        throws <- c(throws, pins)

        if (throws[length(throws)-1] + throws[length(throws)] < 10) {
          # If not a spare, break the loop
          break
        }
      }

      pins <- as.integer(readline(prompt = "Enter number of pins knocked down in throw 3: "))
      if (pins < 0 || pins > 10) {
        cat("Invalid input! Number of pins knocked down must be between 0 and 10.\n")
        return(NULL)
      }
      throws <- c(throws, pins)
    } else {
      # For frames 1 to 9
      pins <- as.integer(readline(prompt = paste("Enter number of pins knocked down in throw 1 of frame", frame, ": ")))
      if (pins < 0 || pins > 10) {
        cat("Invalid input! Number of pins knocked down must be between 0 and 10.\n")
        return(NULL)
      }
      throws <- c(throws, pins)

      pins2_prompt <- paste("Enter number of pins knocked down in throw 2 of frame", frame, ": ")
      if (pins < 10) {
        pins2_max <- 10 - pins
        pins <- as.integer(readline(prompt = pins2_prompt))
        if (pins < 0 || pins > pins2_max) {
          cat("Invalid input! Number of pins knocked down must be between 0 and", pins2_max, ".\n")
          return(NULL)
        }
        throws <- c(throws, pins)
      } else {
        pins <- as.integer(readline(prompt = pins2_prompt))
        throws <- c(throws, 0)  # Automatically add 0 for the second throw if strike
      }
    }

    frame <- frame + 1
  }

  return(throws)
}

# Main function to run the game for two players
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

# Run the game
play_bowling_game()
