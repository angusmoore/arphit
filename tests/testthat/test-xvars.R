context("X vars")

dfdata <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
tsdata <- ts(dfdata, start = c(2000,1), frequency = 4)
tibbledata <- tibble::as_tibble(dfdata)


dfdata <- handledata(NULL, dfdata, NULL)$data
tsdata <- handledata(NULL, tsdata, NULL)$data
tibbledata <- handledata(NULL, tibbledata, NULL)$data

# Default handle ts data
expect_that(handlex(tsdata, NULL), equals(list("1" = as.vector(stats::time(tsdata[["1"]])))))

# Throw error if no x supplied for df or tibble
expect_error(handlex(dfdata, NULL))
expect_error(handlex(tibbledata, NULL))

# Supply x value for each panel
expect_that(handlex(dfdata, x = list("1" = "date")), equals(list("1" = lubridate::decimal_date(dfdata[["1"]][, "date"]))))
expect_that(handlex(tibbledata, x = list("1" = "date")), equals(list("1" = tibbledata[["1"]][, "date"])))

# Supply an x for all panels
expect_that(handlex(dfdata, x = "date"), equals(list("1" = lubridate::decimal_date(dfdata[["1"]][, "date"]))))
expect_that(handlex(tibbledata, x = "date"), equals(list("1" = tibbledata[["1"]][, "date"])))

# supply multiple datasets and multiple x variables
dfdata <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
dfdata2 <- data.frame(date2 = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
multidata <- handledata(NULL, list("1" = dfdata, "2" = dfdata2), NULL)$data
expect_that(handlex(multidata, x = list("1" = "date", "2" = "date2")), equals(list("1" = lubridate::decimal_date(multidata[["1"]]$date), "2" = lubridate::decimal_date(multidata[["2"]]$date2))))

tsdata <- ts(dfdata, start = c(2000,1), frequency = 4)
multidata2 <- handledata(NULL, list("1" = dfdata, "2" = tsdata), NULL)$data
expect_that(handlex(multidata2, x = list("1" = "date")), equals(list("1" = lubridate::decimal_date(multidata[["1"]]$date), "2" = as.vector(stats::time(multidata2[["2"]])))))

# categorical x data
catdata <- handledata(NULL, data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE), NULL)$data
expect_that(handlex(catdata, "x"), equals(list("1" = letters[1:5])))

# Warn if supply timeseries data and an x variable
expect_warning(handlex(handledata(NULL, tsdata, NULL)$data, "x"))
