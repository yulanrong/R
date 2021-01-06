context("Check roll arguments")

fair_die <- device(sides = 1:6, prob = rep(1/6, 6))
set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)

str_die <- device(
  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
set.seed(123)
str_rolls <- roll(str_die, times = 20)

test_that("check_times with ok vectors", {

  expect_true(check_times(20))
  expect_true(check_times(2))
  expect_error(check_times(-10))
  expect_error(check_times('times'))
})

test_that("roll fails with invalid class of device", {

  expect_error(roll(c(1, 2, 3), times = 10))
})

test_that("roll with ok vectors", {
  expect_equal(class(fair_50rolls), 'rolls')
  expect_equal(class(str_rolls), 'rolls')
})

