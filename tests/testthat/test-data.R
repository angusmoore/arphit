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
