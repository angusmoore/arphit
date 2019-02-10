context("qplot")
set.seed(42)

test_that("Simple smoke tests of qplot", {
  data <- ts(data.frame(y=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data), NA)
  data <- ts(data.frame(y=rnorm(10),y2=rnorm(10),y3=rnorm(10)), frequency = 4, start = 2000)
  expect_error(agg_qplot(data, series = c("y","y2")), NA)
  expect_error(agg_qplot(data, series = c("y","y2"), bars = TRUE), NA)
  expect_error(agg_qplot(data, xlim = c(2000,2010),ylim=list(min=-10,max=10,nsteps=5),legend=TRUE,pch=19), NA)
  expect_error(agg_qplot(data, pch=NULL), NA)
})

test_that("Errors for wrong x vars", {
  data <- data.frame(x=1:10,y=rnorm(10))
  skip("This is throwing the wrong error at the moment. Will be fixed by #223.")
  expect_error(agg_qplot(data, x="x1"),
               "x1 is not in your data for panel 1",
               fixed = TRUE)
  expect_error(agg_qplot(data),
               "Cannot add layer. You have not specified an x aesthetic (and there was not one to inherit).",
               fixed = TRUE)
})

