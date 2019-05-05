context("Axes")
set.seed(42)

## Y axes ===================

# needed set up
fakedata <-
  data.frame(
    date = seq.Date(
      from = as.Date("2000-01-01"),
      by = "year",
      length = 10
    ),
    "a" = 1:10,
    "b" = 1:10,
    "c" = 1:10,
    "d" = 1:10
  )

# Basic axis guessing
test_that("Basic axis guessing", {
  p <- arphitgg(fakedata, agg_aes(x=date,y=a))+agg_line()
  expect_true(
    check_graph(p, "axes-basic-ylim-guess")
  )
})

# manually set y axis
test_that("Manually set y axis", {
  p <- arphitgg(fakedata, agg_aes(x=date,y=a))+agg_line()+agg_ylim(1,2,5)
  expect_true(
    check_graph(p, "axes-manual-ylim")
  )

  # apply to all axes on mutlipanel
  p <- arphitgg(fakedata, agg_aes(x=date,y=a),layout="4b2",dropxlabel = TRUE)+agg_line()+agg_ylim(1,2,5)
  expect_true(
    check_graph(p, "axes-manual-ylim-multipanel-all")
  )

  # manually set LHS/RHS
  p <- arphitgg(fakedata)+
    agg_line(agg_aes(x=date,y=a), panel = "1")+agg_ylim(1,2,5, panel = "1") +
    agg_line(agg_aes(x=date,y=b), panel = "2")+agg_ylim(0,12,5,panel = "2")
  expect_true(
    check_graph(p, "axes-manual-ylim-lhs-rhs")
  )

  # Test for passing in a single ylim to apply to all axes in qplot
  sublist <- list(min = 0, max = 1, nsteps = 5)
  expect_that(apply_ylim_to_panels(sublist), equals(
    list(
      "1" = sublist,
      "2" = sublist,
      "3" = sublist,
      "4" = sublist,
      "5" = sublist,
      "6" = sublist,
      "7" = sublist,
      "8" = sublist
    )
  ))
})

# Matching other side axis on horizontal layouts
test_that("Match axes on horizontal layouts",{
  p <- arphitgg(fakedata, layout="2h") +
    agg_line(agg_aes(x=date,y=a), panel = "4") +
    agg_ylim(-10, 10, 3, panel = "4")
  expect_true(
    check_graph(p, "axes-match-axes-2h-right")
  )

  p <- arphitgg(fakedata, layout="2h") +
    agg_line(agg_aes(x=date,y=a), panel = "3") +
    agg_ylim(-10, 10, 3, panel = "3")
  expect_true(
    check_graph(p, "axes-match-axes-2h-left")
  )

  p <- arphitgg(fakedata, layout="3b2") +
    agg_line(agg_aes(x=date,y=a), panel = "3") +
    agg_ylim(-10, 10, 3, panel = "3")
  expect_true(
    check_graph(p, "axes-match-axes-3b2")
  )
})

# Check that sanity checks fail if pass in bad values
test_that("Sanity checks", {
  expect_error(agg_qplot(fakedata, x = "date", ylim = list("min" = 1, "nsteps" = 3)),
               "You did not supply a max ylimit.")
  expect_error(agg_qplot(fakedata, x = "date", ylim = list("max" = 1, "nsteps" = 3)),
               "You did not supply a min ylimit.")

  expect_error({
    p <- arphitgg() + agg_ylim(0, 5, 1)
    print(p)
  },
  "The y-limit you supplied has fewer than 2 points.",
  fixed = TRUE)

  expect_error(
    agg_qplot(fakedata, x = "date", ylim = list("min" = 1, "max" = 3)),
    "The y-limit you supplied has fewer than 2 points (or you forgot to supply nsteps).",
    fixed = TRUE
  )

  expect_error(agg_qplot(data.frame(x=1:10,y=1:10), ylim = c(0,10,5), x="x"), "ylim should be a list")
})


# Ensure y-scale creator works even if NAs in data (#39)
test_that("Y-scale creator", {
  data <- data.frame(x = rnorm(10), y = 100*rnorm(10))
  data[4, "y"] <- NA
  p <- arphitgg(data, agg_aes(x=x,y=y))+agg_point()
  expect_true(
    check_graph(p, "axes-y-scale-with-NAs")
  )


  ## Automatic y-limits based only on visible data (#210)
  data_scatter <- data.frame(x=c(1,2,3,5,9,10),y=c(1000,1000,1000,4,4,4))
  data_categorical <- data.frame(x=letters[1:6],y=c(1000,1000,1000,4,4,4))
  data_time <- data.frame(x=seq.Date(as.Date("2000-06-01"), by = "quarter", length.out = 6),
                          y=c(1000,1000,1000,4,4,4))
  p <- arphitgg(data_scatter, agg_aes(x=x,y=y))+agg_point()+agg_xlim(5,11)
  expect_true(
    check_graph(p, "axes-restrict-x-y-guess-scatter")
  )

  p <- arphitgg(data_categorical, agg_aes(x=x,y=y))+agg_point()+agg_xlim(4,7)
  expect_true(
    check_graph(p, "axes-restrict-x-y-guess-categorical")
  )

  p <- arphitgg(data_time, agg_aes(x=x,y=y))+agg_point()+agg_xlim(2001,2002)
  expect_true(
    check_graph(p, "axes-restrict-x-y-guess-ts")
  )

  # Correct placement of y labels when ticks lead to more decimal places (#44)
  expect_equal(createscale(0.15, 0.25, 5), c(0.15, 0.175, 0.2, 0.225, 0.25))

  # Incorrect rounding of labels (#81)
  foo <- createscale(-0.2, 0.4, 4)
  expect_true(foo[2] == 0)

})


## Default scale ==============

# Test that scale create the right thing
test_that("Default scale", {
  expect_that(createscale(0,3,4), equals(0:3))
  expect_that(createscale(0,4,3), equals(c(0,2,4)))

  # Test that default scales are sensible for line series
  for (i in 1:100) {
    # Just do this 100 times to get lots of different scales and check they are all fine
    data <- data.frame(y=rnorm(10))
    scale <- defaultscale(max(data), min(data))
    expect_true(scale$min <= min(data))
    expect_true(scale$max >= max(data))
    expect_true(scale$nsteps <= max(PERMITTEDSTEPS))
    expect_true(scale$nsteps >= min(PERMITTEDSTEPS))
  }

  # Check that default scales are sensible for stacked _bar_ series (#147)
  # With line series
  data <- data.frame(x=rep(1:5),y=1:5,z=6:10)
  p <- arphitgg(data, agg_aes(x=x))+agg_col(agg_aes(y=y))+agg_col(agg_aes(y=z), stacked = TRUE)
  expect_true(
    check_graph(p, "axes-stacked-bar")
  )

  p <- p + agg_line(agg_aes(y=z))
  expect_true(
    check_graph(p, "axes-stacked-w-line")
  )
})

## X-axes scale ================

# Check x lim for time series charts
test_that("Basic x axis", {
  p <- arphitgg(fakedata, agg_aes(x=date,y=a)) + agg_line()
  expect_true(
    check_graph(p, "axes-x-axis-ts")
  )

  # Check x lim conforming for categorical data
  catdata <- data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE)
  p <- arphitgg(catdata, agg_aes(x=x,y=y))+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-axes")
  )

  # Check x lim conforming for numerical categorical data
  catdata1 <- data.frame(x = 2001:2005, y = 1:5)
  catdata2 <- data.frame(x = c(2,4,6,8,10), y = 1:5)
  p <- arphitgg(catdata1, agg_aes(x=x,y=y))+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-numeric-1")
  )

  p <- arphitgg(catdata2, agg_aes(x=x,y=y))+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-numeric-2")
  )

  # X lim conforming for scatter graph data
  set.seed(42)
  scatter <- data.frame(x = runif(100), y = runif(100))
  p <- arphitgg(scatter, agg_aes(x=x,y=y))+agg_point()
  expect_true(
    check_graph(p, "axes-x-scatter")
  )

  # Labels respecting x limits for scatter graphs (#273)
  set.seed(42)
  foo <- data.frame(x=rnorm(10),y=rnorm(10))
  p <- arphitgg(foo, agg_aes(x=x,y=y))+agg_point()+agg_xlim(-0.9,1)
  expect_true(check_graph(p, "axes-x-scatter-xlimits"))
})

test_that("Warning for different limits",{
  p <- arphitgg(fakedata, agg_aes(x = date, y = a)) +
    agg_line(panel = "1") + agg_point(panel = "2") +
    agg_xlim(2000, 2009, panel = "1") + agg_xlim(2001, 2010, panel = "2")
  expect_warning(
    print(p),
    "Panels 1 and 2 have differing x limits. This may lead to confusing graphs. Be careful!"
  )
})

test_that("Label steps", {
  # Tests for restrictlabels and labelstep
  start <- 1990
  for (end in 1991:2050) {
    ticks <- start:end
    expect_true(length(restrictlabels(ticks, 1)) <= 8)
    expect_true(length(restrictlabels(ticks, 1/2)) <= 4)
    expect_true(length(restrictlabels(ticks, 1/3)) <= 3)
  }

  # Tests for much longer label steps
  start <- 1500
  for (end in 1991:2050) {
    ticks <- start:end
    expect_true(length(restrictlabels(ticks, 1)) <= 8)
    expect_true(length(restrictlabels(ticks, 1/2)) <= 4)
    expect_true(length(restrictlabels(ticks, 1/3)) <= 3)
  }
})

# Restricting x labels for categorical graphs
test_that("Restricted x axis", {
  catdata1 <- data.frame(x = 1:10, y = 1:10)
  catdata2 <- data.frame(x = letters[1:10], y = 1:10, stringsAsFactors = FALSE)

  p <- arphitgg(catdata1, agg_aes(x=x,y=y), showallxlabels = TRUE)+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-numeric-showall")
  )

  p <- arphitgg(catdata1, agg_aes(x=x,y=y), showallxlabels = FALSE)+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-numeric-dontshowall")
  )

  p <- arphitgg(catdata2, agg_aes(x=x,y=y), showallxlabels = TRUE)+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-nonnumeric-showall")
  )

  p <- arphitgg(catdata2, agg_aes(x=x,y=y), showallxlabels = FALSE)+agg_line()
  expect_true(
    check_graph(p, "axes-categorical-x-nonnumeric-dontshowall")
  )
})

# Insufficient x label steps available (#145)
library(dplyr)
library(tidyr)
test_that("Miscellaneous x axis tests", {
  dates <- c("1911-06-01","1925-06-01","1936-06-01","1947-06-01","1958-06-01",
             "1969-08-01","1980-02-01","1991-02-01","2002-02-01","2013-02-01")

  data <- data.frame(date = as.Date(dates), Males = 1:length(dates), Females = 1:length(dates))
  data <- mutate(data, age_group = "15-24") %>%
    bind_rows(mutate(data, age_group = "25-54")) %>%
    bind_rows(mutate(data, age_group = "55+")) %>%
    gather(key = sex, value = PR, Males, Females)

  p <- data %>%
    arphitgg(agg_aes(x=date,y=PR,group=sex,facet=age_group), layout = "3v") +
    agg_line()
  expect_true(
    check_graph(p, "axes-insufficient-x-steps-145")
  )
})

## Unit handling ===============
test_that("Units", {
  p <- arphitgg() + agg_units("foo")
  expect_true(
    check_graph(p, "axes-y-units-set-all")
  )

  p <- arphitgg() + agg_units("foo", "1") + agg_units("bar", "2")
  expect_true(
    check_graph(p, "axes-y-units-set-specific")
  )

  p <- arphitgg()+agg_units("foo", panel = "1")
  expect_true(check_graph(p, "axes-y-units-set-1"))
})

## x axis units =================

test_that("X axis units", {
  set.seed(42)
  scatter <- data.frame(x = runif(100), y = runif(100))
  p <- arphitgg(scatter, agg_aes(x=x,y=y))+agg_point() +agg_xunits("index")
  expect_true(
    check_graph(p, "axes-x-units")
  )

  p <- arphitgg(scatter, agg_aes(x=x,y=y), layout = "2b2")+agg_point(panel = c("1","2","3","4")) +agg_xunits("index")
  expect_true(
    check_graph(p, "axes-x-units-2b2")
  )
})

## Axis labels ===============

test_that("Axis labels", {
  p <- arphitgg() + agg_xaxislabel("foo")+agg_yaxislabel("bar")
  expect_true(
    check_graph(p, "axes-axislabels")
  )
  p <- arphitgg(layout = "2v") + agg_xaxislabel("foo","1")+agg_yaxislabel("bar","1")
  expect_true(
    check_graph(p, "axes-axislabels-2v")
  )
})

## Axis duplication ====================

test_that("Axis duplication", {
  data <- data.frame(x=1:10,y=1:10)
  p <- arphitgg(data, agg_aes(x=x,y=y), layout = "4b2") +
    agg_line(panel = "1") +
    agg_line(panel = "3") +
    agg_line(panel = "5") +
    agg_line(panel = "7")
  expect_true(check_graph(p, "axes-duplicate-left-to-right"))
  p <- arphitgg(data, agg_aes(x=x,y=y), layout = "4b2") +
    agg_line(panel = "2") +
    agg_line(panel = "4") +
    agg_line(panel = "6") +
    agg_line(panel = "8")
  expect_true(check_graph(p, "axes-duplicate-right-to-left"))
  p <- arphitgg(data, agg_aes(x=x,y=y), layout = "3v") +
    agg_line(panel = "1")
  expect_true(check_graph(p, "axes-duplicate-3v-left"))
  p <- arphitgg(data, agg_aes(x=x,y=y), layout = "3v") +
    agg_line(panel = "2")
  expect_true(check_graph(p, "axes-duplicate-3v-center"))
  p <- arphitgg(data, agg_aes(x=x,y=y), layout = "3v") +
    agg_line(panel = "3")
  expect_true(check_graph(p, "axes-duplicate-3v-right"))
})

## Non-year frequency x axis =================

test_that("Non-year frequency x axes", {
  # Decades
  data <- data.frame(dates = seq.Date(as.Date("1960-03-01"),by = "quarter", length.out = 100),
                     y = 1:100)
  p <- arphitgg(data, agg_aes(x=dates,y=y)) + agg_line() + agg_xlim(1956, 2020)
  expect_true(check_graph(p, "axes-x-decades"))

  p <- arphitgg(data, agg_aes(x=dates,y=y), layout = "2v") + agg_line() + agg_xlim(1951, NA)
  expect_true(check_graph(p, "axes-x-decades-2v"))

  # Quarters
  data <- data.frame(dates = seq.Date(as.Date("2000-03-01"),by="quarter", length.out = 8), y = 1:8)
  p <- arphitgg(data, agg_aes(x=dates,y=y)) + agg_line()
  expect_true(check_graph(p, "axes-x-quarters"))

  # Months
  data <- data.frame(dates = seq.Date(as.Date("2000-06-01"),by="month", length.out = 10), y = 1:10)
  p <- arphitgg(data, agg_aes(x=dates,y=y)) + agg_line() + agg_xlim(2000.333333,2001.25)
  expect_true(check_graph(p, "axes-x-months"))
})

## Last year padding ====================

test_that("Last year padding", {
  data <- data.frame(date = seq.Date(as.Date("2000-03-01"),by="quarter",to=as.Date("2004-12-01")),
                     y = 1:20)
  p <- arphitgg(data, agg_aes(x=date,y=y)) + agg_line()
  expect_true(check_graph(p, "axes-lastyearpadding-basic"))

  # Disable last year padding by setting manual x limits
  p <- arphitgg(data, agg_aes(x=date,y=y)) + agg_line() + agg_xlim(NA, 2005)
  expect_true(check_graph(p, "axes-lastyearpadding-disable"))

  # Don't add padding for short time series graphs
  data <- data.frame(date = seq.Date(as.Date("2000-01-01"),by="month",to=as.Date("2001-12-01")),
                     y = 1:24)
  p <- arphitgg(data, agg_aes(x=date,y=y)) + agg_line()
  expect_true(check_graph(p, "axes-lastyearpadding-shortts"))

  # Decades
  data <- data.frame(date = seq.Date(as.Date("2000-01-01"),by="year",to=as.Date("2050-01-01")),
                     y = 1:51)
  p <- arphitgg(data, agg_aes(x=date,y=y)) + agg_line()
  expect_true(check_graph(p, "axes-lastyearpadding-decade"))

  # test that adding lots of padding doesn't throw off x labels
  foo <- data.frame(x=seq.Date(from=as.Date("1980-01-01"),by="year",length.out=29),y=1:29)
  p <- arphitgg(foo, agg_aes(x=x,y=y)) + agg_line()
  expect_true(check_graph(p, "axes-lastyearpadding-no-labels"))

  # Ensure padding is calculated based on manual x limits
  data <- data.frame(date=seq.Date(as.Date("1870-01-01"),by="year",length.out=130),y=1:130)
  p <- arphitgg(data,agg_aes(x=date,y=y))+agg_line() + agg_xlim(1996,NA)
  expect_true(check_graph(p, "axes-lastyearpadding-calculate-w-lowerbound-nopadding"))

  p <- arphitgg(data,agg_aes(x=date,y=y))+agg_line() + agg_xlim(1970,NA)
  expect_true(check_graph(p, "axes-lastyearpadding-calculate-w-lowerbound-somepadding", 0.98))
})
