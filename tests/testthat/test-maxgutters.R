test_that("Player 1 has more gutters than Player 2", {
  player1_rolls <- c(0, 0, 9, 0, 3, 0, 2, 0, 0, 6, 0, 0, 8, 0, 0, 0, 0, 0, 10, 2, 8)
  player2_rolls <- c(0, 4, 6, 0, 0, 10, 6, 2, 2, 0, 7, 2, 0, 9, 0, 8, 2, 3, 0, 4)
  expect_output(max_gutters(player1_rolls, player2_rolls),
                "Player 1 had the most gutters in the game with 13 gutters.")
})

test_that("Player 2 has more gutters than Player 1", {
  player1_rolls <- c(0, 4, 0, 6, 3, 0, 4, 5, 0, 3, 0, 7, 0, 0, 10, 0, 2, 0, 10, 2, 0)
  player2_rolls <- c(0, 0, 0, 3, 0, 0, 4, 0, 0, 0, 0, 0, 7, 0, 0, 3, 5, 0, 0, 0, 0)
  expect_output(max_gutters(player1_rolls, player2_rolls),
                "Player 2 had the most gutters in the game with 16 gutters.")
})

test_that("Both players have the same number of gutters", {
  player1_rolls <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  player2_rolls <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  expect_output(max_gutters(player1_rolls, player2_rolls),
                "Both players had the same number of gutters in the game with 20 gutters each.")
})
