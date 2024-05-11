test_that("player_throws handles incorrect number of throws input", {
  player_rolls <- player_throws(player_no = c(1, 2, 3),
                                throws = list(
                                  c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
                                  c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
                                  c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
                                  ))
  actual_output <- capture.output(bowling_winner(player_rolls))
  expect_equal(actual_output, "Player 1 is the winner")
})
