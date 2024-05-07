# Test case for when both players have null rolls
test_that("Both players must have rolls", {
  expect_error(bowling_leaderboard(NULL, NULL), "Both players must have rolls.")
})

# Test case for when Player 1 wins
test_that("Player 1 wins", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  player2_rolls <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  expect_equal(bowling_leaderboard(player1_rolls, player2_rolls), "Player 1")
})

# Test case for when Player 2 wins
test_that("Player 2 wins", {
  player1_rolls <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  player2_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 8,1)
  expect_equal(bowling_leaderboard(player1_rolls, player2_rolls), "Player 2")
})

# Test case for tie
test_that("It's a tie", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 9,0)
  player2_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 9,0)
  expect_equal(bowling_leaderboard(player1_rolls, player2_rolls), "Tie")
})
