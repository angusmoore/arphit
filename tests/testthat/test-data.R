context("Data handling")

## Tests for types of data
expect_that(is.acceptable.data(data.frame(x1 = rnorm(10))), is_true())
expect_that(is.acceptable.data(tibble::tibble(x1 = rnorm(10))), is_true())
expect_that(is.acceptable.data(utils::data), is_false())
expect_that(is.acceptable.data(1), is_false())
expect_that(is.acceptable.data(utils::data()), is_false())

## Handle data
data <- ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)

# basic case - just passing data, should only plot on first panel
series <- list("1" = c("x1", "x2", "x3", "x4"))
expect_that(handledata(NULL, data, NULL), equals(list(data = list("1" = data), series = series)))

# Put series on different panels
series <- list("1" = c("x1", "x2"), "2" = c("x3", "x4"))
expect_that(handledata(series, data, NULL), equals(list(data = list("1" = data, "2" = data), series = series)))

# Different data sets, but don't specify series for each data set
data2 <- ts(data.frame(x5 = rnorm(12), x6 = rnorm(12), x7 = rnorm(12, sd = 10), x8 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)

series <- list("1" = c("x1", "x2", "x3", "x4"), "2" = c("x5", "x6", "x7", "x8"))
expect_that(handledata(NULL, list("1" = data, "2" = data2), NULL), equals(list(data = list("1" = data, "2" = data2), series = series)))

# Different data sets and specify series for both panels
series <- list("1" = c("x1", "x2"), "2" = c("x5", "x6"))
expect_that(handledata(series, list("1" = data, "2" = data2), NULL), equals(list(data = list("1" = data, "2" = data2), series = series)))

# Different data sets and specify series for only one panel
inseries <- list("1" = c("x1", "x2"))
outseries <- list("1" = c("x1", "x2"), "2" = c("x5", "x6", "x7", "x8"))
expect_that(handledata(inseries, list("1" = data, "2" = data2), NULL), equals(list(data = list("1" = data, "2" = data2), series = outseries)))

# Removing x series
df <- data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
expect_that(handledata(NULL, df, "x1"), equals(list(data = list("1" = df), series = list("1" = c("x2", "x3", "x4")))))

expect_that(handledata(NULL, list("1" = df, "2" = df), list("1" = "x1", "2" = "x2")), equals(list(data = list("1" = df, "2" = df), series = list("1" = c("x2", "x3", "x4"), "2" = c("x1", "x3", "x4")))))

## Errors
# Common error of passing in the data function...
expect_error(handledata(inseries, list("1" = utils::data(), "2" = data2), NULL))
expect_error(handledata(NULL, utils::data, NULL))
expect_error(arphit.tsgraph(utils::data))

# Requesting non-existent data series
error_series <- list("1" = "x1", "2" = "x5")
expect_error(handledata(error_series, data, NULL))
expect_error(arphit.tsgraph(data, series = error_series))
expect_error(handledata(error_series, list("1" = data, NULL)))
expect_error(arphit.tsgraph(list("1" = data), series = error_series))
expect_error(handledata(list("1" = "x5", "2" = "x5"), list("1" = data, "2" = data2), NULL))
expect_error(handledata(list("1" = "x1", "2" = "x1"), list("1" = data, "2" = data2), NULL))

# Passing multiple x variable for one dataset
expect_error(handledata(NULL, data, list("1" = "x1", "2" = "x2")))

# Request non existent x variable
expect_error(handledata(NULL, data, "foo"))

# not passing a list for the series list
expect_error(handledata("foo", data, NULL))
expect_error(handledata(("1" = "x1"), data, NULL))
expect_error(arphit(data, series = ("1" = "x1")))

# Passing in non-finite values
infinitedata <- data.frame(cat = letters[1:5], x1 = rnorm(10), x2 = rnorm(10))
infinitedata[4, "x1"] <- Inf
expect_error(arphit(infinitedata, x = "cat"), "Series x1 in panel 1 contains non-finite values")
infinitets <- ts(data.frame(x1 = rnorm(10), x2 = rnorm(10)), start = c(2000,1), frequency = 4)
infinitets[4, "x2"] <- Inf
expect_error(arphit(infinitets), "Series x2 in panel 1 contains non-finite values")
