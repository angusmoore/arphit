context("Data handling")

## Tests for types of data
expect_that(is.acceptable.data(data.frame(x1 = rnorm(10))), is_true())
expect_that(is.acceptable.data(tibble::tibble(x1 = rnorm(10))), is_true())
expect_that(is.acceptable.data(utils::data), is_false())
expect_that(is.acceptable.data(1), is_false())
expect_that(is.acceptable.data(utils::data()), is_false())

## Handle data
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

expect_equal(get_series_names(
  list("1" = data, "2" = data),
  list("1" = "agg_time", "2" = "agg_time")
),
list(
  "1" = c("x1", "x2", "x3", "x4"),
  "2" = c("x1", "x2", "x3", "x4")
))
# And for qplot, giving just one data
expect_equal(get_series_names(
  conformdata(data,
              "1",
              list(
                "1" = c("x1", "x2"), "2" = c("x3", "x4")
              )),
  list("1" = "agg_time", "2" = "agg_time")
),
list("1" = c("x1", "x2"), "2" = c("x3", "x4")))


# Different data sets
data2 <-
  data.frame(
    x5 = rnorm(12),
    x6 = rnorm(12),
    x7 = rnorm(12, sd = 10),
    x8 = rnorm(12, sd = 5),
    agg_time = seq.Date(
      from = as.Date("2001-03-1"),
      by = "quarter",
      length.out = 12
    )
  )

expect_equal(get_series_names(
  list("1" = data, "2" = data2),
  list("1" = "agg_time", "2" = "agg_time")
),
list(
  "1" = c("x1", "x2", "x3", "x4"),
  "2" = c("x5", "x6", "x7", "x8")
))

# Removing x series
df <-
  data.frame(
    x1 = rnorm(12),
    x2 = rnorm(12),
    x3 = rnorm(12, sd = 10),
    x4 = rnorm(12, sd = 5)
  )

expect_equal(get_series_names(list("1" = df, "2" = df), list("1" = "x1", "2" = "x2")), list(
  "1" = c("x2", "x3", "x4"),
  "2" = c("x1", "x3", "x4")
))

## Errors
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
expect_error(
  get_series_names(list(
    "foo" = data.frame(), "1" = data.frame(x = 1:10)
  ), NULL),
  "Invalid index in data sets. Indexes must correspond to panel numbers between 1 and 8."
)

# Request non existent x variable
expect_error(get_series_names(data, "foo"))

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
