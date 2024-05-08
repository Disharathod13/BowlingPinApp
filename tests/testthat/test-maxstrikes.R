test_that("Player 2 has more strikes than Player 1", {
  player1_rolls <- c(10, 3, 4, 10, 5, 10, 7, 2, 3, 4, 10, 10, 10, 6, 7, 8, 9, 1, 2, 3)
  player2_rolls <- c(10, 10, 4, 3, 5, 10, 10, 2, 3, 4, 10, 10, 10, 6, 7, 8, 9, 1, 2, 3)
  expect_output(player_with_most_strikes(player1_rolls, player2_rolls),
                "Player 2 had the most strikes in the game with 7 strikes.")
})

test_that("Player 1 has more strikes than Player 2", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  player2_rolls <- c(10, 3, 4, 10, 5, 10, 7, 2, 3, 4, 10, 10, 10, 6, 7, 8, 9, 1, 2, 3)
  expect_output(player_with_most_strikes(player1_rolls, player2_rolls),
                "Player 1 had the most strikes in the game with 12 strikes.")
})

test_that("Both players have the same number of strikes", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  player2_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  expect_output(player_with_most_strikes(player1_rolls, player2_rolls),
                "Both players had the same number of strikes in the game with 12 strikes each.")
})
