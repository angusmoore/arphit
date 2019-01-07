context("Axes")

# needed set up
fakeseries1 <- c("a", "b")
fakeseries2 <- c("c", "d")
onesided <- list("1" = fakeseries1, "2" = NULL)
twosided <- list("1" = fakeseries1, "2" = fakeseries2)
twosided_oneeach <- list("1" = "a", "2" = "b")
fakedata <-
  list("1" = data.frame(
    agg_time = seq.Date(
      from = as.Date("2000-01-01"),
      by = "year",
      length = 10
    ),
    "a" = 1:10,
    "b" = 1:10
  ))
twosideddata <-
  list(
    "1" = data.frame(
      agg_time = seq.Date(
        from = as.Date("2000-01-01"),
        by = "year",
        length = 1
      ),
      "a" = 1:10,
      "b" = 1:10,
      "c" = 1:10,
      "d" = 1:10
    ),
    "2" = data.frame(
      agg_time = seq.Date(
        from = as.Date("2000-01-01"),
        by = "year",
        length = 1
      ),
      "a" = 1:10,
      "b" = 1:10,
      "c" = 1:10,
      "d" = 1:10
    )
  )
twosided_oneeachdata <-
  list("1" = (data.frame("a" = 1:10, "b" = 1:10)),
       "2" = (data.frame("a" = 1:10, "b" = 1:10)))

context("Y-axes scale")
shouldbe <- list("1" = list("min" = 0, "max" = 12, "nsteps" = 5), "2" = list("min" = 0, "max" = 12, "nsteps" = 5))
expect_that(ylimconform(onesided, list(), fakedata, list(), "1"), equals(shouldbe))

sublist <- list("min" = 1, "max" = 2, "nsteps" = 3)
expect_that(ylimconform(onesided, list("1" = sublist), fakedata, list(), "1"), equals(list("1" = sublist, "2" = sublist)))

should_be <- list("1" = sublist, "2" = list(min = 0, max = 12, nsteps = 5))
expect_that(ylimconform(twosided_oneeach, list("1" = sublist), twosided_oneeachdata, list(), "1"), equals(should_be))

ylim <- ylimconform(onesided, list(), fakedata, list(), "1")
expect_that(handleticks(fakedata, ylim), equals(list("1" = c(0,3,6,9,12), "2" = c(0,3,6,9,12))))
panel2b2 <- list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4"))

# Test for passing in a single ylim to apply to all axes
sublist <- list("min" = 1, "max" = 2, "nsteps" = 3)
largerdata <-  ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)
expect_that(ylimconform(panel2b2, sublist, largerdata, list(), "2b2"), equals(list("1" = sublist, "2" = sublist, "3" = sublist, "4" = sublist)))

# Matching other side axis
expect_equal(ylimconform(
  list("1" = NULL, "2" = "x1"),
  list("2" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("2" = largerdata),
  list(),
  "1"
)[["1"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list(
    "1" = NULL,
    "2" = NULL,
    "3" = NULL,
    "4" = "x1"
  ),
  list("4" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("4" = largerdata),
  list(),
  "2h"
)[["3"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list(
    "1" = NULL,
    "2" = NULL,
    "3" = NULL,
    "4" = NULL,
    "5" = NULL,
    "6" = "x1"
  ),
  list("6" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("6" = largerdata),
  list(),
  "3b2"
)[["5"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list(
    "1" = NULL,
    "2" = NULL,
    "3" = NULL,
    "4" = NULL,
    "5" = NULL,
    "6" = NULL,
    "7" = "x1",
    "8" = NULL
  ),
  list("7" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("7" = largerdata),
  list(),
  "4b2"
)[["8"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list(
    "1" = NULL,
    "2" = NULL,
    "3" = NULL,
    "4" = NULL,
    "5" = NULL,
    "6" = NULL,
    "7" = NULL,
    "8" = "x1"
  ),
  list("8" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("8" = largerdata),
  list(),
  "4h"
)[["7"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list("1" = NULL,
       "2" = NULL,
       "3" = "x1"),
  list("3" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("3" = largerdata),
  list(),
  "3v"
)[["2"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

expect_equal(ylimconform(
  list("1" = "x1",
       "2" = NULL,
       "3" = NULL),
  list("1" = list(
    min = -10,
    max = 10,
    nsteps = 3
  )),
  list("1" = largerdata),
  list(),
  "3v"
)[["2"]],
list(
  min = -10,
  max = 10,
  nsteps = 3
))

# Check that sanity checks fail if pass in bad values
expect_error(ylimconform(onesided, list("1" = list("min" = 1, "nsteps" = 3)), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("min" = 1, "nsteps" = 3), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("1" = list("max" = 1, "nsteps" = 3)), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("max" = 1, "nsteps" = 3), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("1" = list("min" = 1, "max" = 3)), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("min" = 1, "max" = 3), fakedata, list(), "1"))
expect_error(ylimconform(onesided, list("1" = list("min" = 1, "max" = 2, "nsteps" = 1)), fakedata, list(), "1"))
expect_error(agg_qplot(data.frame(x=1:10,y=1:10), ylim = c(0,10,5), x="x"), "Ylim should be a list")

context("Default scale")
# Test that scale create the right thing
expect_that(createscale(0,3,4), equals(0:3))
expect_that(createscale(0,4,3), equals(c(0,2,4)))

# Test that default scales are sensible for line series
for (i in 1:100) {
  # Just do this 100 times to get lots of different scales and check they are all fine
  data <- data.frame(y=rnorm(10))
  scale <- defaultscale(data, NULL, FALSE)
  expect_that(scale$min <= min(data),is_true())
  expect_that(scale$max >= max(data),is_true())
  expect_that(scale$nsteps <= max(PERMITTEDSTEPS),is_true())
  expect_that(scale$nsteps >= min(PERMITTEDSTEPS),is_true())
}

# Check that default scales are sensible for stacked _bar_ series (#147)

# With line series
data <- data.frame(x=rep(1:5),y=1:5,z=6:10)
scale <- defaultscale(data, c("y","z"), TRUE)
expect_true(scale$min <= 0)
expect_true(scale$max >= 15)
data <- data.frame(x=rep(1:5),y=-1:-5,z=-6:-10)
scale <- defaultscale(data, c("y","z"), TRUE)
expect_true(scale$max >= 5)
expect_true(scale$min <= -15)

# Without line series
data <- data.frame(y=1:5,z=6:10)
scale <- defaultscale(data, c("y","z"), TRUE)
expect_true(scale$min <= 0)
expect_true(scale$max >= 15)
data <- data.frame(y=-1:-5,z=-6:-10)
scale <- defaultscale(data, c("y","z"), TRUE)
expect_true(scale$max >= 0)
expect_true(scale$min <= -15)

context("X-axes scale")
# Check x lim conforming for time series data
xvars <- get_x_values(fakedata, list("1" = "agg_time"))
expect_that(xlimconform(onesided, list(), xvars, fakedata), equals(list("1" = c(2000,2010), "2" = c(2000,2010))))

expect_equal(xlimconform(onesided, c(-10,0), xvars, fakedata), list("1" = c(-10,0), "2" = c(-10,0)))

xlim <- xlimconform(onesided, NULL, xvars, fakedata)
x <- get_x_values(fakedata, list("1" = "agg_time"))
xlabs <- handlexlabels(onesided, xlim, x, fakedata, "1")
expect_equal(xlabs, list("1" = list(at = c(2001.5,2003.5,2005.5,2007.5,2009.5), labels = c(2001,2003,2005,2007,2009), ticks = 2000:2009), "2" = list(at = c(2001.5,2003.5,2005.5,2007.5,2009.5), labels = c(2001,2003,2005,2007,2009), ticks = 2000:2009)))
expect_warning(xlimconform(twosided, list("1" = c(2000,2010), "2" = c(2001,2009)), twosideddata))

# Tests for restrictlabels and labelstep
start <- 1990
for (end in 1991:2050) {
  ticks <- start:end
  expect_true(length(restrictlabels(ticks, 1)) <= 8)
  expect_true(length(restrictlabels(ticks, 1/2)) <= 4)
  expect_true(length(restrictlabels(ticks, 1/3)) <= 2)
}
# Tests for much longer label steps
start <- 1500
for (end in 1991:2050) {
  ticks <- start:end
  expect_true(length(restrictlabels(ticks, 1)) <= 8)
  expect_true(length(restrictlabels(ticks, 1/2)) <= 4)
  expect_true(length(restrictlabels(ticks, 1/3)) <= 2)
}

# Check x lim conforming for categorical data
catdata <- list("1" = data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE))
catpanels <- list("1" = c("y"), "2" = NULL)
xvar <- get_x_values(catdata, list("1" = "x"))
expect_equal(xlimconform(catpanels, NULL, xvar, catdata), list("1" = c(1, 6), "2" = c(1, 6)))

# Check x lim conforming for numerical categorical data
catdata1 <- list("1" = data.frame(x = 2001:2005, y = 1:5))
catdata2 <- list("1" = data.frame(x = c(2,4,6,8,10), y = 1:5))
xvar1 <- get_x_values(catdata1, list("1" = "x"))
xvar2 <- get_x_values(catdata2, list("1" = "x"))
xlim1 <- xlimconform(catpanels, NULL, xvar1, catdata1)
expect_that(xlim1, equals(list("1" = c(2001, 2006), "2" = c(2001, 2006))))
xlim2 <- xlimconform(catpanels, NULL, xvar2, catdata2)
expect_that(xlim2, equals(list("1" = c(2,12), "2" = c(2,12))))
xlabs1 <- handlexlabels(catpanels, xlim1, xvar1, catdata1, "1", NULL)
expect_that(xlabs1[["1"]]$at, equals(c(2001.5, 2002.5, 2003.5, 2004.5, 2005.5)))
expect_that(xlabs1[["1"]]$labels, equals(2001:2005))
expect_that(xlabs1[["1"]]$ticks, equals(2001:2005))
xlabs2 <- handlexlabels(catpanels, xlim2, xvar2, catdata2, "1", NULL)
expect_that(xlabs2[["1"]]$at, equals(c(3,5,7,9,11)))
expect_that(xlabs2[["1"]]$labels, equals(c(2,4,6,8,10)))
expect_that(xlabs2[["1"]]$ticks, equals(c(2,4,6,8,10)))

# Restricting x labels for categorical graphs
catdata1 <- list("1" = data.frame(x = 1:10, y = 1:10))
xvar1 <- get_x_values(catdata1, list("1" = "x"))
xlim1 <- xlimconform(catpanels, NULL, xvar1, catdata1)

catdata2 <- list("1" = data.frame(x = letters[1:10], y = 1:10, stringsAsFactors = FALSE))
xvar2 <- get_x_values(catdata2, list("1" = "x"))
xlim2 <- xlimconform(catpanels, NULL, xvar2, catdata2)

xlabs1 <- handlexlabels(catpanels, xlim1, xvar1, catdata1, "1", TRUE)
expect_equal(xlabs1[["1"]]$labels, 1:10)
xlabs1 <- handlexlabels(catpanels, xlim1, xvar1, catdata1, "1", FALSE)
expect_equal(xlabs1[["1"]]$labels, c(2,4,6,8,10))

xlabs2 <- handlexlabels(catpanels, xlim2, xvar2, catdata2, "1", TRUE)
expect_equal(xlabs2[["1"]]$labels, letters[1:10])
xlabs2 <- handlexlabels(catpanels, xlim2, xvar2, catdata2, "1", FALSE)
expect_equal(xlabs2[["1"]]$labels, c("b","d","f","h","j"))

# X lim conforming for scatter graph data
scatter <- data.frame(x = runif(100), y = runif(100))
scatter <- list("1" = scatter)
xvar <- get_x_values(scatter, list("1" = "x"))
scatterpanels <- list("1" = "y", "2" = NULL)
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
nadata <- list("1" = data)
napanels <- list("1" = "y", "2" = NULL)
expect_false(isTRUE(all.equal(ylimconform(napanels, list(), nadata, list(), "1")[["1"]], list(min = -1, max = 2, nsteps = 4))))

# Correct placement of y labels when ticks lead to more decimal places (#44)
expect_equal(createscale(0.15, 0.25, 5), c(0.15, 0.175, 0.2, 0.225, 0.25))

# X axis unit handling
context("x axis units")
expect_equal(handlexunits(onesided, NULL), list("1" = "%", "2" = "%"))
expect_equal(handlexunits(onesided, list("1" = "index")), list("1" = "index", "2" = "%"))
expect_equal(handlexunits(onesided, "index"), list("1" = "index", "2" = "index"))
panels2b2 <- list("1" = "a","2"="c","3"="b","4"=c("d","e"))
expect_equal(handlexunits(panels2b2, "index"), list("1" = "index", "2" = "index", "3" = "index", "4" = "index"))

# Axis labels
expect_equal(handleaxislabels(list("1" = "foo"), onesided), list("1" = "foo"))
expect_equal(handleaxislabels("foo", onesided), list("1" = "foo", "2" = "foo"))

# Incorrect rounding of labels (#81)
foo <- createscale(-0.2, 0.4, 4)
expect_true(foo[2] == 0)

# Insufficient x label steps available (#145)
library(dplyr)
library(tidyr)
dates <- c("1911-06-01","1925-06-01","1936-06-01","1947-06-01","1958-06-01",
           "1969-08-01","1980-02-01","1991-02-01","2002-02-01","2013-02-01")

data <- data.frame(date = dates, Males = rnorm(length(dates)), Females = rnorm(length(dates)))
data$date <-  as.Date(data$date)
data <- mutate(data, age_group = "15-24") %>%
  bind_rows(mutate(data, age_group = "25-54")) %>%
  bind_rows(mutate(data, age_group = "55+")) %>%
  gather(key = sex, value = PR, Males, Females)

p <- data %>%
  arphitgg(agg_aes(x=date,y=PR,group=sex,facet=age_group), layout = "3v") +
  agg_line()
expect_error(
  agg_draw(p),
  NA
)

# Poor handling of x labels for non-integer x limits in time series
expect_equal(xlabels.ts(c(2008.008,2009.008), "1")$at %% 1, 0.5)
expect_true(all((xlabels.ts(c(2008.008,2019.008), "1")$at %% 1) ==0.5))

# Showing labels waaay off of panels for high frequency data
for (i in 1:1000) {
  x <- runif(1)
  expect_lte(xlabels.ts(c(2008 + x, 2009 + x), "1")$at, 2009 + x)
  expect_gte(xlabels.ts(c(2008 + x, 2009 + x), "1")$at, 2008 + x)
}
