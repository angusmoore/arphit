# needed set up
fakeseries1 <- c("a","b")
fakeseries2 <- c("c","d")
onesided <- handlepanels(fakeseries1, NULL, "1")
twosided <- handlepanels(list("1" = fakeseries1, "2" = fakeseries2), NULL, "1")
fakedata <- as.ts(data.frame("a" = 1:10, "b" = 1:10))

context("Y-axes scale")
shouldbe <- list("1" = list("min" = 0, "max" = 12, "nsteps" = 5), "2" = list("min" = 0, "max" = 12, "nsteps" = 5))
expect_that(ylimconform(onesided, NULL, fakedata, "1"), equals(shouldbe))

sublist <- list("min" = 1, "max" = 2, "nsteps" = 3)
expect_that(ylimconform(onesided, list("1" = sublist), fakedata, "1"), equals(list("1" = sublist, "2" = sublist)))

ylim <- ylimconform(onesided, NULL, fakedata, "1")
expect_that(handleticks(fakedata, onesided, ylim), equals(list("1" = c(0,3,6,9,12), "2" = c(0,3,6,9,12))))

expect_error(ylimconform(onesided, list("1" = list("min" = 1, "max" = 2, "nsteps" = 1)), fakedata, "1"))

context("Default scale")
# Test that scale create the right thing
expect_that(createscale(0,3,4), equals(0:3))
expect_that(createscale(0,4,3), equals(c(0,2,4)))

# Test that default scales are sensible
for (i in 1:100) {
  # Just do this 100 times to get lots of different scales and check they are all fine
  data <- rnorm(10)
  scale <- defaultscale(data)
  expect_that(scale$min <= min(data),is_true())
  expect_that(scale$max >= max(data),is_true())
  expect_that(scale$nsteps <= max(PERMITTEDSTEPS),is_true())
  expect_that(scale$nsteps >= min(PERMITTEDSTEPS),is_true())
}

context("X-axes scale")
for (i in 1:100) {
  xlim <- sort(sample(1979:2017,2))
  xlab <- xlabels(xlim)
  expect_that(xlab$at[1], equals(min(xlim)+0.5))
  expect_that(xlab$labels[1], equals(min(xlim)))
  expect_that(length(xlab$labels), equals(length(xlab$at)))
  expect_that(length(xlab$labels) <= MAXXYEARS, is_true())
}

expect_that(xlimconform(onesided, NULL, fakedata), equals(list("1" = c(1,10), "2" = c(1,10))))
xlim <- xlimconform(onesided, NULL, fakedata)
xlabels <- handlexlabels(onesided, xlim)
test_that(xlabels, equals(1.5:10.5))

expect_warning(xlimconform(twosided, list("1" = c(2000,2010), "2" = c(2001,2009)), fakedata))
