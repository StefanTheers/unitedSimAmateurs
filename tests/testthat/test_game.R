library("unitedSimAmateurs")


test_that("round2_equal", {
  expect_equal(round2(1), 1)
  expect_equal(round2(-5), -5)
  expect_equal(round2(101), 101)
  expect_equal(round2(1e4), 1e4)
  expect_equal(round2(1e8), 1e8)
  expect_equal(round2(1e12), 1e12)
  expect_equal(round2(1e16), 1e16)
})
test_that("round2_floor", {
  expect_equal(round2(0.1), 0)
  expect_equal(round2(1/4), 0)
  expect_equal(round2(1/3), 0)
  expect_equal(round2(0.4999), 0)
  expect_equal(round2(1.4999), 1)
  expect_equal(round2(9.4999), 9)
  expect_equal(round2(1e4+0.4999), 1e4)
  expect_equal(round2(1e12+0.4999), 1e12)
})
test_that("round2_ceiling", {
  expect_equal(round2(0.9), 1)
  expect_equal(round2(3/4), 1)
  expect_equal(round2(2/3), 1)
  expect_equal(round2(0.5), 1)
  expect_equal(round2(0.5), 1)
  expect_equal(round2(1.5), 2)
  expect_equal(round2(9.5), 10)
  expect_equal(round2(1e4+0.5), 1e4+1)
  expect_equal(round2(1e12+0.5), 1e12+1)
})



# Check some known results from C++ nflo (for various p and "expected" vs "sample"):
test_that("game_examples_expected_p5", {
  res <- game(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(1, 2))
  res <- game(x1 = c(t=0, a=0, v=2, m=2, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 1))
  res <- game(x1 = c(t=0, a=1, v=1, m=1, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 14/15))
  res <- game(x1 = c(t=1, a=0, v=1, m=1, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 2*13/14))
})
test_that("game_examples_expected_p30", {
  res <- game(x1 = c(t=10, a=5, v=0, m=0, s=0), x2 = c(t=0, a=0, v=5, m=10, s=15), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 16 * 10/15 * 4/14))
})
test_that("game_examples_expected_p65", {
  res <- game(x1 = c(t=10, a=10, v=12, m=9, s=4), x2 = c(t=0, a=0, v=11, m=27, s=27), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 16 * 5/15 * 4/14))
})
test_that("game_examples_expected_round2", {
  # This one is nasty because mercantile rounding makes a difference here, i.e. the use of round2()!
  res <- game(x1 = c(t=0, a=9, v=20, m=31, s=11), x2 = c(t=0, a=3, v=33, m=26, s=15), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c((3+1) * 1 * 12/15, 6 * 1 * 6/15))
})



#' Helper function that tests full-time results if one team wins
#' @param x1 lineup x1
#' @param x2 lineup x2
#' @param reps integer(1), number of replications
#' @param eps numeric(1), precision
#' @param expected numeric(2), the expected result of x1 vs x2
#' @return testthat::expect_true return
testGameSample <- function(x1, x2, reps = 1e3, eps = 0.1, expected = c(1, 2), expect_fun = expect_ft) {
  res <- replicate(reps, game(x1 = x1, x2 = x2, method = "sample", etps = TRUE), simplify = FALSE)
  res2   <- colMeans(do.call(rbind, res), na.rm = TRUE)
  resFT  <- colMeans(do.call(rbind, lapply(res, function(x) attr(x, "fullTimeScore"))),  na.rm = TRUE)
  resET  <- colMeans(do.call(rbind, lapply(res, function(x) attr(x, "extraTimeScore"))), na.rm = TRUE)
  resPen <- colMeans(do.call(rbind, lapply(res, function(x) attr(x, "penalties"))),      na.rm = TRUE)
  expect_fun(res = res, res2 = res2, resFT = resFT, resET = resET, resPen = resPen, expected = expected, eps = eps)
}
expect_ft <- function(res, res2, resFT, resET, resPen, expected, eps)
  expect_true(abs(resFT[1] - expected[1]) + abs(resFT[2] - expected[2]) <= eps)
expect_et <- function(res, res2, resFT, resET, resPen, expected, eps)
  expect_true(abs(resET[1] - expected[1]) + abs(resET[2] - expected[2]) <= eps)
expect_pen <- function(res, res2, resFT, resET, resPen, expected, eps) {
  # TODO
  expect_true(TRUE)
}




test_that("game_examples_sample_p5", {
  set.seed(2020)
  testGameSample(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), expected = c(1, 2))
  testGameSample(x1 = c(t=0, a=0, v=2, m=2, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), expected = c(0, 1))
  testGameSample(x1 = c(t=0, a=1, v=1, m=1, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), expected = c(0, 14/15))
  testGameSample(x1 = c(t=1, a=0, v=1, m=1, s=1), x2 = c(t=0, a=0, v=1, m=1, s=3), expected = c(0, 2*13/14))
})
test_that("game_examples_sample_p30", {
  set.seed(2020)
  testGameSample(x1 = c(t=10, a=5, v=0, m=0, s=0), x2 = c(t=0, a=0, v=5, m=10, s=15), expected = c(0, 16 * 10/15 * 4/14))
})
test_that("game_examples_sample_p65", {
  set.seed(2020)
  testGameSample(x1 = c(t=10, a=10, v=12, m=9, s=4), x2 = c(t=0, a=0, v=11, m=27, s=27), expected = c(0, 16 * 5/15 * 4/14))
})



# Look at draws (plus etra-time and penalties):
test_that("game_examples_expected_draw", {
  res <- game(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), method = "expected")
  expect_equal(attr(res, "fullTimeScore"), c(0, 0))
  # TODO: Check expected penalty results!
})
test_that("game_examples_sample_draw", {
  set.seed(2020)
  testGameSample(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), expect_fun = expect_ft,  expected = c(0, 0))
  testGameSample(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), expect_fun = expect_et,  expected = c(0, 0))
  testGameSample(x1 = c(t=0, a=0, v=1, m=3, s=1), x2 = c(t=0, a=0, v=1, m=3, s=1), expect_fun = expect_pen, expected = c(0, 0))
  # TODO: Add more tests!
})





