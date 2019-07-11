context("Data handling")
set.seed(42)
## Set up some data
data <-
  data.frame(
    x1 = rnorm(12),
    x2 = rnorm(12),
    x3 = rnorm(12, sd = 10),
    x4 = rnorm(12, sd = 5),
    agg_time = seq.Date(
      from  = as.Date("2001-03-01"),
      by = "quarter",
      length.out = 12
    )
  )

## Tests for types of data
test_that("Acceptable data types", {
  expect_true(is.acceptable.data(data.frame(x1 = 1:10)))
  expect_true(is.acceptable.data(tibble::tibble(x1 = 1:10)))
  expect_false(is.acceptable.data(utils::data))
  expect_false(is.acceptable.data(1))
  expect_false(is.acceptable.data(utils::data()))
})

## Errors ==================

test_that("Errors", {
  # Common error of passing in the data function...
  expect_error({
    p <- arphitgg(utils::data, agg_aes(x = x, y = y)) + agg_line()
    print(p)
  },
  "Data is of unsupported type (you passed in function)",
  fixed = TRUE)
  expect_error(agg_qplot(utils::data, x = "x"),
               "Data is of unsupported type (you passed in function)",
               fixed = TRUE)

  # Non-existent panel
  expect_error(
      arphitgg(data, agg_aes(x=agg_time,y=x1))+agg_line(panel="foo"),
      "Panel identifier 'foo' is invalid. Panels must be between 1 and 8."
  )

  # Passing in non-finite values
  infinitedata <-
    data.frame(cat = letters[1:5],
               x1 = rnorm(10),
               x2 = rnorm(10))
  infinitedata[4, "x1"] <- Inf
  expect_error(agg_qplot(infinitedata, x = "cat"),
               "Series x1 contains non-finite values")
  infinitets <-
    ts(data.frame(x1 = rnorm(10), x2 = rnorm(10)),
       start = c(2000, 1),
       frequency = 4)
  infinitets[4, "x2"] <- Inf
  expect_error(agg_qplot(infinitets),
               "Series x2 contains non-finite values")

  # Error if pass in data with no rows (#86)
  foo <- data.frame(x = numeric(), y = numeric())
  expect_error(agg_qplot(foo, x = "x"), "Series y has no observations.")
  expect_error(arphitgg(foo, agg_aes(x = x, y = y)) + agg_line(), "Series y has no observations.")

  # Bar graph with duplicate x
  data <- data.frame(x=c(1,2,2,3),y=c(1,2,3,4))
  expect_error(
    print(arphitgg(data, agg_aes(x=x,y=y))+agg_col()),
    "Series y invalid. Bar graphs cannot have duplicate entries for x values.",
    fixed = TRUE
  )
})

## Zoo and XTS data (#129) =================

test_that("zoo and xts", {
  set.seed(42)
  foo <-
    xts::xts(data.frame(y=rnorm(10),y2=rnorm(10)),
             order.by = seq.Date(
               from = as.Date("2000-08-01"),
               length.out = 10,
               by = "quarter"
             ))
  bar <- zoo::as.zoo(foo)

  p <- arphitgg(foo) + agg_line(agg_aes(y=y)) + agg_line(agg_aes(y=y2))
  expect_error(agg_qplot(foo),NA)
  expect_true(check_graph(p, "data-xts"))

  p <- arphitgg(bar) + agg_line(agg_aes(y=y)) + agg_line(agg_aes(y=y2))
  expect_error(agg_qplot(bar),NA)
  expect_true(check_graph(p, "data-xts")) # Should be identical to the xts version
})


## Placement of bars ================

test_that("Bar graph placement", {
  set.seed(42)
  foo <- data.frame(date=seq.Date(from = as.Date("2000-01-01"),by="month",length.out=12*4),y=rnorm(12*4))
  p <- arphitgg(foo, agg_aes(x=date,y=y))+agg_line()+agg_col()+agg_xlim(2000,2004)
  expect_true(check_graph(p, "data-bar-placement"))

  # 157 - widening x values
  foo <- data.frame(date = c(as.Date("2000-03-01"),
                      as.Date("2000-09-01"),
                      as.Date("2001-03-01"),
                      as.Date("2001-09-01"),
                      as.Date("2001-12-01"),
                      as.Date("2002-03-01"),
                      as.Date("2002-06-01")),
             y = 1:7)
  p <- arphitgg(foo, agg_aes(x=date,y=y)) + agg_point() + agg_col()
  expect_true(check_graph(p, "data-bar-widen-x"))

  # Widening week and days - problematic as there aren't consistent numbers of them in a year
  # 352
  set.seed(42)
  dates <- seq(as.Date("2013/1/1"), as.Date("2016/1/1"), "weeks")
  data <- data.frame(date = dates,
                     y = runif(length(dates)))
  p <- arphitgg(data, agg_aes(x=date,y=y,group=lubridate::year(dates)))+agg_col()+
    agg_vline(2014,panel="1")+agg_vline(2015,panel="1")
  expect_true(check_graph(p, "data-bar-widen-x-weeks"))

  dates <- seq(as.Date("2013/1/1"), as.Date("2015/1/1"), "days")
  data <- data.frame(date = dates,
                     y = runif(length(dates)))
  p <- arphitgg(data, agg_aes(x=date,y=y,group=lubridate::year(date)))+agg_col()+
    agg_vline(2014,panel="1")
  expect_true(check_graph(p, "data-bar-widen-x-days", 0.985))
})
