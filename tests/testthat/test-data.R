data <- ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)

error_series <- list("1" = "x1", "2" = "x5")

expect_error(sanitycheckdata(error_series, data))
expect_error(arphit.tsgraph(data, series = error_series))

fine_series <- list("1" = "x1", "2" = "x2")
expect_error(sanitycheckdata(fine_series, data), NA)
