context("xvars")

## Frequency guessing =========================
test_that("Frequency guessing", {
  days <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2010-01-01"), by = "day")
  months <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2010-01-01"), by = "month")
  quarters <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2010-01-01"), by = "quarter")
  years <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2010-01-01"), by = "year")
  expect_equal(frequencyof(days), 1/365)
  expect_equal(frequencyof(months), 1/12)
  expect_equal(frequencyof(quarters), 1/4)
  expect_equal(frequencyof(years), 1)
  # Bad behaviour with non recognised frequencies of data
  expect_equal(frequencyof(seq.Date(from=as.Date("2000-01-01"),by="week",length.out = 100)), 1/(365/7))
  expect_null(frequencyof(seq.POSIXt(from=ISOdate(1999,1,1),by="hour",length.out = 100)))
  # duplicate dates
  duplicate_dates <- as.Date(c("2000-03-01","2000-06-01","2000-09-01","2000-09-01","2000-12-01","2001-03-01"))
  expect_equal(frequencyof(duplicate_dates), 1/4)
  # single date
  expect_equal(frequencyof(as.Date("2018-06-01")), 1)
})

## Time series graphs at different frequencies ==================
set.seed(42)
test_that("Time series graphs at different frequencies", {
  # weekly data
  data <-
    data.frame(date = seq.Date(
      from = as.Date("2000-01-01"),
      by = "week",
      length.out = 100
    ),
    y = rnorm(100))
  p <- arphitgg(data, agg_aes(x = date, y = y)) + agg_line()
  expect_true(check_graph(p, "xvars-weekly"))

  # semi annual data
  data <-
    data.frame(date = as.Date(c("2000-03-01","2000-09-01","2001-03-01","2001-09-01","2002-03-1")),
               y = rnorm(5))
  p <- arphitgg(data, agg_aes(x = date, y = y)) + agg_line()
  expect_true(check_graph(p, "xvars-semi-annual"))

  # Irregularly spaced
  data <- data.frame(year = c(1991, 2001, 2006, 2011, 2016), y = 1:5)
  data$year <- as.Date(paste0(data$year, "-01-01"))
  p <- arphitgg(data, agg_aes(x=year,y=y)) + agg_line()
  expect_true(
    check_graph(p, "xvars-irregular-spaced-ts")
  )

  # Monthly
  foo <- ts(data.frame(y = rnorm(100)), frequency = 12, start = c(2000,1))
  p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
  expect_true(check_graph(p, "xvars-monthly"))

  # Daily
  foo <- ts(data.frame(y = rnorm(1000)), frequency = 365.25, start = c(2000,1))
  p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
  expect_true(check_graph(p, "xvars-daily1"))

  foo <- ts(data.frame(y = rnorm(1000)), frequency = 365, start = c(2000,1))
  p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
  expect_true(check_graph(p, "xvars-daily2"))

  foo <- data.frame(x = seq(from = as.Date("2000-01-01"),length.out=1000,by="day"),y=rnorm(1000))
  p <- arphitgg(foo, agg_aes(x=x,y=y))+agg_line()
  expect_true(check_graph(p, "xvars-daily3"))

  skip("Hourly doesn't look any good at the moment. will be fixed by #227")
  # hourly data
  data <-
    data.frame(date = seq.POSIXt(from=ISOdate(1999,1,1),by="hour",length.out = 100),
               y = rnorm(100))
  p <- arphitgg(data, agg_aes(x = date, y = y)) + agg_line()
  expect_true(check_graph(p, "xvars-hourly"))
})

## Error handling ==================
# Test if don't specify x variable for qplot
test_that("Error handling", {
  expect_error(
    agg_qplot(data.frame(y=1:10)),
    "You did not specify an x variable for panel 1"
  )
})


## Single observation graphs =======================

test_that("Single observation graphs", {
  # Test #159 - frequencyof failing for singleton observation time series
  foo <- data.frame(date= as.Date("2018-08-01"),y=c(4,7,2),group=letters[1:3])
  p <- arphitgg(foo, agg_aes(x=date,y=y,group=group)) + agg_col()
  expect_true(check_graph(p, "xvars-singleton-159"))

  # Test #171 - poor handling of singleton column graphs
  foo <-
    data.frame(
      x = "z",
      y = c(4,3,1),
      group = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )
  p <- arphitgg(foo, agg_aes(x = x, y = y, group = group)) + agg_col()
  expect_true(check_graph(p, "xvars-singleton-171"))

  # Test for singleton numeric
  foo <-
    data.frame(
      x = 4,
      y = c(8,5,1),
      group = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )
  p <- arphitgg(foo, agg_aes(x = x, y = y, group = group)) + agg_col()
  expect_true(check_graph(p, "xvars-singleton-numeric"))

  # Test for singleton without groups
  foo <- data.frame(x = 4, y = 5)
  p <- arphitgg(foo, agg_aes(x = x, y = y)) + agg_col()
  expect_true(check_graph(p, "xvars-singleton-no-group-numeric-cat"))

  foo <- data.frame(x = "A", y = 2, stringsAsFactors = FALSE)
  p <- arphitgg(foo, agg_aes(x = x, y = y)) + agg_col()
  expect_true(check_graph(p, "xvars-singleton-no-group-cat"))

  # Similar to 171 (though different cause), failure for singleton numeric x categories
  foo <- data.frame(x=1,y=c(5,2,7),group=c("a","b","c"),stringsAsFactors = FALSE)
  p <- arphitgg(foo, agg_aes(x = x, y = y, group = group)) + agg_point()
  expect_true(check_graph(p, "xvars-singleton-point"))
})

## Miscellaneous tests =================

test_that("Miscellaneous", {
  # Test #37
  # Error if x variable has NA values
  data <- data.frame(x = c(1,2,3,NA,4),y = c(4,3,7,1,2))
  p <- arphitgg(data, agg_aes(x=x,y=y))+agg_point()
  expect_true(check_graph(p, "xvars-x-nas"))

  # Test #143
  # Correctly handle if dates are not in order
  foo <- data.frame(x = c(2017,2015,2012,2006,2009),y=10:14)
  foo$x <-  lubridate::make_date(foo$x, 1, 1)
  p <- arphitgg(foo, agg_aes(x=x,y=y)) + agg_line()
  expect_true(
    check_graph(p, "xvars-unordered-dates")
  )
})
