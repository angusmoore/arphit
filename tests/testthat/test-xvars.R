context("X vars")

dfdata <- list("1" = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                                length.out = 12,
                                                by = "quarter"),
                     x1 = rnorm(12), x2 = rnorm(12),
                     x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)))
tsdata <- conformdata(ts(dfdata[["1"]], start = c(2000,1), frequency = 4), "1", NULL)
tibbledata <- list("1" = tibble::as_tibble(dfdata[["1"]]))


dfdata <- handledata(NULL, dfdata, NULL)$data
tsdata <- handledata(NULL, tsdata, NULL)$data
tibbledata <- handledata(NULL, tibbledata, NULL)$data

# Throw error if no x supplied for df or tibble
expect_error(get_x_values(dfdata, NULL),
             "Have not supplied an x variable for panel 1 and cannot guess it because it is not a time series.")
expect_error(get_x_values(tibbledata, NULL),
             "Have not supplied an x variable for panel 1 and cannot guess it because it is not a time series.")
# Throw error if supply non existent x variable
expect_error(get_x_values(tibbledata, list("1" = "foo")))

# Supply x value for each panel
offsetdates <- seq(from = 2000.125,
                   by = 0.25,
                   length.out = 12)
expect_equal(get_x_values(dfdata, x = list("1" = "date")),
             list(
               "1" = offsetdates,
               "1ts" = TRUE,
               "1freq" = 0.25
             ))
expect_equal(get_x_values(tibbledata, x = list("1" = "date")),
             list(
               "1" = offsetdates,
               "1ts" = TRUE,
               "1freq" = 0.25
             ))
# Supply an x for all panels
expect_equal(get_x_values(dfdata, x = list("1" = "date")),
             list(
               "1" = offsetdates,
               "1ts" = TRUE,
               "1freq" = 0.25
             ))
expect_equal(get_x_values(tibbledata, x = list("1" = "date")),
             list(
               "1" = offsetdates,
               "1ts" = TRUE,
               "1freq" = 0.25
             ))

# supply multiple datasets and multiple x variables
offsetdates <- seq(from = 2000.125,
                   by = 0.25,
                   length.out = 12)
dfdata <-
  data.frame(
    date = seq.Date(
      from = as.Date("2000-01-01"),
      length.out = 12,
      by = "quarter"
    ),
    x1 = rnorm(12),
    x2 = rnorm(12),
    x3 = rnorm(12, sd = 10),
    x4 = rnorm(12, sd = 5)
  )
dfdata2 <-
  data.frame(
    date2 = seq.Date(
      from = as.Date("2000-01-01"),
      length.out = 12,
      by = "quarter"
    ),
    x1 = rnorm(12),
    x2 = rnorm(12),
    x3 = rnorm(12, sd = 10),
    x4 = rnorm(12, sd = 5)
  )

multidata <-
  handledata(NULL, list("1" = dfdata, "2" = dfdata2), NULL)$data
expect_equal(
  get_x_values(multidata, x = list("1" = "date", "2" = "date2")),
  list(
    "1" = offsetdates,
    "1ts" = TRUE,
    "1freq" = 0.25,
    "2" = offsetdates,
    "2ts" = TRUE,
    "2freq" = 0.25
  )
)

# Same x variable across multiple datasets
data1 <- data.frame(x1 = 1:10, x2 = rnorm(10))
data2 <- data.frame(x1 = 1:10, x4 = rnorm(10))
expect_error(agg_qplot(data = list("1" = data1, "2" = data2), x = "x1"), NA)

# categorical x data
catdata <- handledata(NULL, list("1" = data.frame(x = letters[1:5], y = 1:5, stringsAsFactors = FALSE)), NULL)$data
expect_that(get_x_values(catdata, list("1" = "x")), equals(list("1" = letters[1:5])))

# Frequency guessing
days <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "day")
months <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "month")
quarters <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "quarter")
years <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "year")
expect_equal(frequencyof(days), 1/365)
expect_equal(frequencyof(months), 1/12)
expect_equal(frequencyof(quarters), 1/4)
expect_equal(frequencyof(years), 1)

# Test old bug from old frequency guessing causing incorrect labels with irregularly spaced data
data <- data.frame(year = c(1991, 2001, 2006, 2011, 2016), y = rnorm(5))
data$year <- as.Date(paste0(data$year, "-01-01"))
expect_that(get_x_values(handledata(NULL, list("1" = data), list("1" = "year"))$data, list("1" = "year"))[["1"]],
            equals(c(1991.5, 2001.5, 2006.5, 2011.5, 2016.5)))
# Test if don't specify x variable for qplot
expect_error(
  agg_qplot(data.frame(y=1:10)),
  "You did not specify an x variable for panel 1"
)

# Monthly and daily data
foo <- ts(data.frame(y = rnorm(100)), frequency = 12, start = c(2000,1))
p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
expect_error(
  print(p),
  NA
)

foo <- ts(data.frame(y = rnorm(1000)), frequency = 365.25, start = c(2000,1))
p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
expect_error(
  print(p),
  NA
)

foo <- ts(data.frame(y = rnorm(1000)), frequency = 365, start = c(2000,1))
p <- arphitgg(foo, agg_aes(y=y)) + agg_line()
expect_error(
  print(p),
  NA
)

# Test #37
# Error if x variable has NA values
data <- data.frame(x = c(1,2,3,NA,4),y = c(4,3,7,1,2))
expect_error(agg_qplot(data, x="x"), NA)

# Test #143
# Correctly handle if dates are not in order
foo <- data.frame(x = c(2017,2015,2012,2009,2006),y=rnorm(5))
foo$x <-  lubridate::make_date(foo$x, 1, 1)
p <- arphitgg(foo, agg_aes(x=x,y=y)) + agg_line()
expect_error(
  print(p),
  NA
)
graphics.off()

# Test #159 - frequencyof failing for singleton observation time series
foo <- data.frame(date= as.Date("2018-08-01"),y=rnorm(3),group=letters[1:3])
p <- arphitgg(foo, agg_aes(x=date,y=y,group=group)) + agg_col()
expect_error(
  print(p),
  NA
)
