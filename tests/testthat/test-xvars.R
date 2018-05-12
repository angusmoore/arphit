context("X vars")

dfdata <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
tsdata <- ts(dfdata, start = c(2000,1), frequency = 4)
tibbledata <- tibble::as_tibble(dfdata)


dfdata <- handledata(NULL, dfdata, NULL)$data
tsdata <- handledata(NULL, tsdata, NULL)$data
tibbledata <- handledata(NULL, tibbledata, NULL)$data

# Default handle ts data
tsoffsetdates <- as.vector(stats::time(tsdata[["1"]]) + 1/(2*stats::frequency(tsdata[["1"]])))
expect_that(handlex(tsdata, NULL), equals(list("1" = tsoffsetdates, "1ts" = TRUE)))

# Throw error if no x supplied for df or tibble
expect_error(handlex(dfdata, NULL))
expect_error(handlex(tibbledata, NULL))
# Throw error if supply non existent x variable
expect_error(handlex(tibbledata, list("1" = "foo")))
expect_error(handlex(tibbledata, "foo"))

# Supply x value for each panel
offsetdates <- lubridate::decimal_date(dfdata[["1"]][, "date"]) + 1/8
expect_that(handlex(dfdata, x = list("1" = "date")), equals(list("1" = offsetdates, "1ts" = TRUE)))
expect_that(handlex(tibbledata, x = list("1" = "date")), equals(list("1" = offsetdates, "1ts" = TRUE)))

# Supply an x for all panels
expect_that(handlex(dfdata, x = "date"), equals(list("1" = offsetdates, "1ts" = TRUE)))
expect_that(handlex(tibbledata, x = "date"), equals(list("1" = offsetdates, "1ts" = TRUE)))

# supply multiple datasets and multiple x variables
offsetdates <- lubridate::decimal_date(seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter")) + 1/8
dfdata <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))
dfdata2 <- data.frame(date2 = seq.Date(from = as.Date("2000-01-01"), length.out = 12, by = "quarter"), x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5))

multidata <- handledata(NULL, list("1" = dfdata, "2" = dfdata2), NULL)$data
expect_that(handlex(multidata, x = list("1" = "date", "2" = "date2")), equals(list("1" = offsetdates, "1ts" = TRUE, "2" = offsetdates, "2ts" = TRUE)))

tsdata <- ts(dfdata, start = c(2000,1), frequency = 4)
multidata2 <- handledata(NULL, list("1" = dfdata, "2" = tsdata), NULL)$data
expect_that(handlex(multidata2, x = list("1" = "date")), equals(list("1" = offsetdates, "1ts" = TRUE, "2" = tsoffsetdates, "2ts" = TRUE)))

# Same x variable across multiple datasets
data1 <- data.frame(x1 = 1:10, x2 = rnorm(10))
data2 <- data.frame(x1 = 1:10, x4 = rnorm(10))
expect_error(agg_qplot(data = list("1" = data1, "2" = data2), x = "x1"), NA)

# categorical x data
catdata <- handledata(NULL, data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE), NULL)$data
expect_that(handlex(catdata, "x"), equals(list("1" = letters[1:5])))

# Warn if supply timeseries data and an x variable
expect_warning(handlex(handledata(NULL, tsdata, NULL)$data, "x"))

# Frequency guessing
days <- lubridate::decimal_date(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "day"))
months <- lubridate::decimal_date(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "month"))
quarters <- lubridate::decimal_date(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "quarter"))
years <- lubridate::decimal_date(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "year"))
expect_that(frequencyof(days), equals(1/365))
expect_that(frequencyof(months), equals(1/12))
expect_that(frequencyof(quarters), equals(1/4))
expect_that(frequencyof(years), equals(1))

# Test old bug from old frequency guessing causing incorrect labels with irregularly spaced data
data <- data.frame(year = c(1991, 2001, 2006, 2011, 2016), y = rnorm(5))
data$year <- as.Date(paste0(data$year, "-01-01"))
expect_that(handlex(handledata(NULL, data, "year")$data, "year")[["1"]], equals(c(1991.5, 2001.5, 2006.5, 2011.5, 2016.5)))

# Test #37
# Error if x variable has NA values
data <- data.frame(x = c(1,2,3,NA,4),y = c(4,3,7,1,2))
expect_error(agg_qplot(data, x="x"), NA)
