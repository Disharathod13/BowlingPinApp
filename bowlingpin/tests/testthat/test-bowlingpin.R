# Test case 1: depicts a situation where both players achieve perfect games, with each frame resulting in a strike.
test_that("both players have perfect games", {
  player1_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  player2_rolls <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  expected_output <- c(300, 300)
  expect_equal(bowlingpin(player1_rolls, player2_rolls), expected_output)
})

# Test case 2: involves a scenario where both players score zero in every frame,  only gutter balls for all frames.
test_that("both players have all gutter balls", {
  player1_rolls <- rep(0, 20)
  player2_rolls <- rep(0, 20)
  expected_output <- c(0, 0)
  actual_output <- bowlingpin(player1_rolls, player2_rolls)
  expect_equal(actual_output, expected_output)
})

