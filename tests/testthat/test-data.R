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
  expect_that(is.acceptable.data(data.frame(x1 = 1:10)), is_true())
  expect_that(is.acceptable.data(tibble::tibble(x1 = 1:10)), is_true())
  expect_that(is.acceptable.data(utils::data), is_false())
  expect_that(is.acceptable.data(1), is_false())
  expect_that(is.acceptable.data(utils::data()), is_false())
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
  expect_error(agg_qplot(utils::data),
               "Data is of unsupported type (you passed in function)",
               fixed = TRUE)

  # Non-existent panel
  p <- arphitgg(data, agg_aes(x=agg_time,y=x1))+agg_line(panel="foo")
  expect_error(
    print(p),
    "Invalid index in data sets. Indexes must correspond to panel numbers between 1 and 8."
  )

  # not passing a list for the series list
  expect_error(
    agg_qplot(data, series = ("1" = "x1")),
    "`series` must be a list mapping panel names to vector of series to be included in that panel."
  )

  # Passing in non-finite values
  infinitedata <-
    data.frame(cat = letters[1:5],
               x1 = rnorm(10),
               x2 = rnorm(10))
  infinitedata[4, "x1"] <- Inf
  expect_error(agg_qplot(infinitedata, x = "cat"),
               "Series x1 in panel 1 contains non-finite values")
  infinitets <-
    ts(data.frame(x1 = rnorm(10), x2 = rnorm(10)),
       start = c(2000, 1),
       frequency = 4)
  infinitets[4, "x2"] <- Inf
  expect_error(agg_qplot(infinitets),
               "Series x2 in panel 1 contains non-finite values")

  # Error if pass in data with no rows (#86)
  foo <- data.frame(x = numeric(), y = numeric())
  expect_error(agg_qplot(foo, x = "x"), "Data in panel 1 has no rows.")
  bar <- arphitgg(foo, agg_aes(x = x, y = y)) + agg_line()
  expect_error(print(bar), "Data in panel 1 has no rows")
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
