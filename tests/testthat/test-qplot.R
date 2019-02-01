context("qplot")
set.seed(42)

test_that("Simple smoke tests of qplot", {
  data <- ts(data.frame(y=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data), NA)
  data <- ts(data.frame(y=rnorm(10),y2=rnorm(10),y3=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data, series = c("y","y2")), NA)
  expect_error(agg_qplot(data, series = c("y","y2"), bars = TRUE), NA)
})

test_that("Errors for wrong x vars", {
  data <- data.frame(x=1:10,y=rnorm(10))
  expect_error(agg_qplot(data, x="x1"),
               "The x variable you specified (x1) is not in your data.",
               fixed = TRUE)
  expect_error(agg_qplot(data),
               "You did not specify an x variable and cannot guess it because your data is not a time series.",
               fixed = TRUE)
})

