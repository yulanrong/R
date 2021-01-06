context("Check device arguments")

weird_die <- device(
  sides = c('i', 'ii', 'iii', 'iv'), prob = rep(1/4, 4))
loaded_die <- device(
  sides = 1:6,
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))

test_that("check_sides with ok vectors", {

  expect_true(check_sides(c('heads', 'tails')))
  expect_true(check_sides(c(1, 2, 3)))
})

test_that("check_sides fails with invalid lengths", {

  expect_error(check_sides(c(1)))
  expect_error(check_sides(c('a')))
})

test_that("check_sides fails with duplicated elements", {

  expect_error(check_sides(c('heads', 'heads')))
})


test_that("check_prob works with ok vectors", {

  expect_true(check_prob(c(0.5, 0.5)))
  expect_true(check_prob(c(0, 1)))
  expect_true(check_prob(c(0.2, 0.3, 0.5)))
})


test_that("check_prob fails with invalid values", {

  expect_error(check_prob(c(-0.5, 0.5)))
  expect_error(check_prob(c(0.5, 1.5)))
  expect_error(check_prob(c(0.2, 0.1)))
  expect_error(check_prob(c(0.5, NA)))
})

test_that("device fails with different length of sides and prob", {

  expect_error(device( sides = c('a', 'b', 'c'), prob = c(0.2, 0.8)))
})

test_that("is.device fails with invalid class", {

  expect_equal(is.device(c(1, 2, 3)), FALSE)
  expect_equal(is.device(weird_die), TRUE)
  expect_equal(is.device(loaded_die), TRUE)
})


