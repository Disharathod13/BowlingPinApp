test_that("both players have perfect games", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  player2_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  expected_output <- c(300, 300)
  expect_equal(bowlingpin(player1_rolls, player2_rolls), expected_output)
})


test_that("both players have all gutter balls", {
  player1_rolls <- rep(0, 20)
  player2_rolls <- rep(0, 20)
  expected_output <- c(0, 0)
  actual_output <- bowlingpin(player1_rolls, player2_rolls)
  expect_equal(actual_output, expected_output)
})

