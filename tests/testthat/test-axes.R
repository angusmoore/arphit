# needed set up
fakeseries1 <- c("a","b")
fakeseries2 <- c("c","d")
onesided <- handlepanels(fakeseries1, "1")
twosided <- handlepanels(list("1" = fakeseries1, "2" = fakeseries2), "1")
twosided_oneeach <- handlepanels(list("1" = "a", "2" = "b"), "1")
fakedata <- handledata(NULL, as.ts(data.frame("a" = 1:10, "b" = 1:10)), NULL)$data
twosideddata <- handledata(list("1" = fakeseries1, "2" = fakeseries2), as.ts(data.frame("a" = 1:10, "b" = 1:10, "c" = 1:10, "d" = 1:10)), NULL)$data
twosided_oneeachdata <- handledata(list("1" = "a", "2" = "b"), as.ts(data.frame("a" = 1:10, "b" = 1:10)), NULL)$data

context("Y-axes scale")
shouldbe <- list("1" = list("min" = 0, "max" = 12, "nsteps" = 5), "2" = list("min" = 0, "max" = 12, "nsteps" = 5))
expect_that(ylimconform(onesided, NULL, fakedata, "1"), equals(shouldbe))

sublist <- list("min" = 1, "max" = 2, "nsteps" = 3)
expect_that(ylimconform(onesided, list("1" = sublist), fakedata, "1"), equals(list("1" = sublist, "2" = sublist)))

should_be <- list("1" = sublist, "2" = list(min = 0, max = 12, nsteps = 5))
expect_that(ylimconform(twosided_oneeach, list("1" = sublist), twosided_oneeachdata, "1"), equals(should_be))

ylim <- ylimconform(onesided, NULL, fakedata, "1")
expect_that(handleticks(fakedata, onesided, ylim), equals(list("1" = c(0,3,6,9,12), "2" = c(0,3,6,9,12))))
panel2b2 <- handlepanels(list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), "2b2")

# Test for passing in a single ylim to apply to all axes
sublist <- list("min" = 1, "max" = 2, "nsteps" = 3)
largerdata <-  ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)
expect_that(ylimconform(panel2b2, sublist, largerdata, "2b2"), equals(list("1" = sublist, "2" = sublist, "3" = sublist, "4" = sublist)))

# Check that sanity checks fail if pass in bad values
expect_error(ylimconform(onesided, list("1" = list("min" = 1, "nsteps" = 3)), fakedata, "1"))
expect_error(ylimconform(onesided, list("min" = 1, "nsteps" = 3), fakedata, "1"))
expect_error(ylimconform(onesided, list("1" = list("max" = 1, "nsteps" = 3)), fakedata, "1"))
expect_error(ylimconform(onesided, list("max" = 1, "nsteps" = 3), fakedata, "1"))
expect_error(ylimconform(onesided, list("1" = list("min" = 1, "max" = 3)), fakedata, "1"))
expect_error(ylimconform(onesided, list("min" = 1, "max" = 3), fakedata, "1"))
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
# Check x lim conforming for time series data
fakedata <- handledata(NULL, fakedata, NULL)$data
xvars <- handlex(fakedata, NULL)
expect_that(xlimconform(onesided, NULL, xvars, fakedata), equals(list("1" = c(1,11), "2" = c(1,11))))

expect_equal(xlimconform(onesided, c(-10,0), xvars, fakedata), list("1" = c(-10,0), "2" = c(-10,0)))

xlim <- xlimconform(onesided, NULL, xvars, fakedata)
x <- handlex(fakedata, NULL)
xlabs <- handlexlabels(onesided, xlim, x, fakedata, "1")
expect_equal(xlabs, list("1" = list(at = c(2.5,4.5,6.5,8.5,10.5), labels = c(2,4,6,8,10), ticks = 1:10)))
expect_warning(xlimconform(twosided, list("1" = c(2000,2010), "2" = c(2001,2009)), twosideddata))

# Tests for restrictlabels and labelstep
start <- 1990
for (end in 1991:2050) {
  ticks <- start:end
  expect_true(length(restrictlabels(ticks, 1)) <= 8)
  expect_true(length(restrictlabels(ticks, 1/2)) <= 4)
  expect_true(length(restrictlabels(ticks, 1/3)) <= 2)
}

# Check x lim conforming for categorical data
catdata <- handledata(NULL, data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE), "x")$data
catpanels <- handlepanels(c("y"), "1")
xvar <- handlex(catdata, "x")
expect_that(xlimconform(catpanels, NULL, xvar, catdata), equals(list("1" = c(1, 6), "2" = c(1, 6))))

# Check x lim conforming for numerical categorical data
catpanels <- handlepanels(c("y"), "1")
catdata1 <- handledata(NULL, data.frame(x = 2001:2005, y = 1:5), "x")$data
catdata2 <- handledata(NULL, data.frame(x = c(2,4,6,8,10), y = 1:5), "x")$data
xvar1 <- handlex(catdata1, "x")
xvar2 <- handlex(catdata2, "x")
xlim1 <- xlimconform(catpanels, NULL, xvar1, catdata1)
expect_that(xlim1, equals(list("1" = c(2001, 2006), "2" = c(2001, 2006))))
xlim2 <- xlimconform(catpanels, NULL, xvar2, catdata2)
expect_that(xlim2, equals(list("1" = c(2,12), "2" = c(2,12))))
xlabs1 <- handlexlabels(catpanels, xlim1, xvar1, catdata1)
expect_that(xlabs1[["1"]]$at, equals(c(2001.5, 2002.5, 2003.5, 2004.5, 2005.5)))
expect_that(xlabs1[["1"]]$labels, equals(2001:2005))
expect_that(xlabs1[["1"]]$ticks, equals(2001:2005))
xlabs2 <- handlexlabels(catpanels, xlim2, xvar2, catdata2)
expect_that(xlabs2[["1"]]$at, equals(c(3,5,7,9,11)))
expect_that(xlabs2[["1"]]$labels, equals(c(2,4,6,8,10)))
expect_that(xlabs2[["1"]]$ticks, equals(c(2,4,6,8,10)))

# X lim conforming for scatter graph data
scatter <- data.frame(x = runif(100), y = runif(100))
scatter <- handledata(NULL, scatter, "x")$data
xvar <- handlex(scatter, "x")
scatterpanels <- handlepanels(c("y"), "1")
expect_that(xlimconform(scatterpanels, NULL, xvar, scatter), equals(list("1" = c(0,1), "2" = c(0,1))))

context("Unit handling")
expect_that(handleunits(onesided, NULL, "1"), equals(list("1" = "%", "2" = "%")))
expect_that(handleunits(onesided, "foo", "1"), equals(list("1" = "foo", "2" = "foo")))
expect_that(handleunits(onesided, list("1" = "foo", "2" = "bar"), "1"), equals(list("1" = "foo", "2" = "foo"))) # because it's only got series on one side overwrite the option
expect_that(handleunits(twosided, list("1" = "foo", "2" = "bar"), "1"), equals(list("1" = "foo", "2" = "bar")))
expect_that(handleunits(onesided, list("1" = "foo"), "1"), equals(list("1" = "foo", "2" = "foo"))) # Duplicate across the first axis
expect_that(handleunits(twosided, list("1" = "foo"), "1"), equals(list("1" = "foo", "2" = "%"))) # Series on RHS, so should get the default

# Ensure y-scale creator works even if NAs in data (#39)
data <- data.frame(x = rnorm(10), y = 100*rnorm(10))
data[4, "y"] <- NA
nadata <- handledata(NULL, data, NULL)$data
napanels <- handlepanels("y", "1")
expect_false(isTRUE(all.equal(ylimconform(napanels, NULL, nadata, "1")[["1"]], list(min = -1, max = 2, nsteps = 4))))

# Correct placement of y labels when ticks lead to more decimal places (#44)
expect_equal(createscale(0.15, 0.25, 5), c(0.15, 0.175, 0.2, 0.225, 0.25))
