test_that("bowlingpin function calculates scores correctly", {
  player_rolls <- player_throws(player_no = c(1, 2, 3),
                                throws = list(
                                  c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
                                  c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
                                  c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
                                ))
  result <- bowlingpin(player_rolls)
  expected_output <- vctrs::new_rcrd(list(
    player_no = c(1, 2, 3),
    throws = list(
      c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
      c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
      c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
    ),
    score = c(92, 79, 89)
  ), class = "throws")
  expect_equal(result, expected_output)
})
