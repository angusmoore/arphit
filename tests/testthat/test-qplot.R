context("qplot")
set.seed(42)

test_that("Simple smoke tests of qplot", {
  data <- ts(data.frame(y=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data), NA)
  data <- ts(data.frame(y=rnorm(10),y2=rnorm(10),y3=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data, series = list("1" = c("y","y2"))), NA)
  expect_error(agg_qplot(data, series = list("1" = c("y","y2")), bars = TRUE), NA)
})

test_that("Errors for wrong x vars", {
  data <- data.frame(x=1:10,y=rnorm(10))
  expect_error(agg_qplot(data, x="x1"),
               "The x variable you supplied for panel 1 (x1) is not a variable in the data you supplied.",
               fixed = TRUE)
  expect_error(agg_qplot(data, x = list("2" = "x1")),
               "Have not supplied an x variable for panel 1 and cannot guess it because it is not a time series.",
               fixed = TRUE)
})

test_that("Errors for paneltitles", {
  data <- data.frame(x=1:10,y=rnorm(10))
  expect_error(agg_qplot(data, x="x", paneltitles = "foo"),
               "`paneltitles` must be a list.")

  skip("This should throw an error but is not")
  expect_error(agg_qplot(data, x="x", panelsubtitles = "foo"),
               "`paneltitles` must be a list.")
})
