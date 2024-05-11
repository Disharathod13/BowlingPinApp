test_that("player_throws handles incorrect number of throws input", {
  expect_error(player_throws(player_no = c(1, 2, 3),
                             throws = list(
                               c(2, 3, 4, 5, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 6, 10),
                               c(2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
                               c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
                             ))
               , "Invalid input! Number of pins knocked down in a frame must be between 0 and 10.")
})



test_that("player_throws handles throws exceeding 10 pins", {
  expect_error(player_throws(player_no = c(1, 2, 3),
                             throws = list(
                               c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 7, 10),
                               c(2, 3, 4, 5, 6, 4, 8, 1, 1, 2, 3, 4, 5, 5, 7, 2, 9, 1, 4, 3),
                               c(1, 2, 3, 4, 5, 3, 1, 8, 9, 1, 2, 3, 4, 5, 2, 7, 8, 1, 10, 0, 10)
                             )))
})



test_that("player_throws handles non-numeric player numbers", {
  expect_error(player_throws(player_no = c(1, 2, "three"), throws = list()))
})


test_that("player_throws handles non-numeric throws", {
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(1:20, 21:40, "abc")))
})


test_that("player_throws handles unequal lengths of player numbers and throws", {
  expect_error(player_throws(player_no = c(1, 2), throws = list(1:20)))
})


test_that("player_throws handles negative throw values", {
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(-1, 2, 3)))
})


test_that("player_throws handles non-numeric score", {
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(1:20), score = "abc"))
})


test_that("player_throws handles score vector length mismatch", {
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(1:20), score = c(1, 2)))
})


test_that("player_throws handles throws with non-integer values", {
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(c(1.5, 2.3, 3.7))))
})


test_that("player_throws handles throws with invalid frame configurations", {
  # less than 10 frames
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(c(1, 2, 3, 4, 5, 6, 7, 8, 9))))

  # only 19 throws
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(c(rep(10, 18), 9, 1))))

  # only 20 throws, no bonus
  expect_error(player_throws(player_no = c(1, 2, 3), throws = list(c(rep(10, 18), 9, 0))))
})



