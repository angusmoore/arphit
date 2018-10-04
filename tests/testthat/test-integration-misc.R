context("Misc integration tests")

# #71 incorrect handling of NAs
bar <- data.frame(x = c(2000:2003), y = c(NA, rnorm(3)), z = c(NA, y = rnorm(3)))
expect_error(agg_qplot(bar, x="x"), NA)
expect_error(agg_qplot(bar, x="x", joined = FALSE), NA)

# Zoo and xts data (#129)
bar <-
  xts::xts(data.frame(y=rnorm(10),y2=rnorm(10)),
           order.by = seq.Date(
             from = as.Date("2000-08-01"),
             length.out = 10,
             by = "quarter"
           ))

expect_error(
  agg_qplot(bar),
  NA
)
expect_error(
  {
    p <- arphitgg(bar) + agg_line(agg_aes(y=y)) + agg_line(agg_aes(y=y2))
    print(p)
  },
  NA
)

bar <- zoo::as.zoo(bar)
expect_error(
  agg_qplot(bar),
  NA
)
expect_error(
  {
    p <- arphitgg(bar) + agg_line(agg_aes(y=y)) + agg_line(agg_aes(y=y2))
    print(p)
  },
  NA
)

# Log scales (#161)

expect_error({
  p <- data.frame(x = 1:10,
                  y = c(11, 20, 40, 90, 11, 14, 90, 15, 15, 16)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "xy") + agg_line() + agg_ylim(10, 90, 5) + agg_xlim(1,10)
  print(p)
},
NA)

expect_error({
  p <- data.frame(x = 1:10,
                  y = c(11, 20, 40, 90, 11, 14, 90, 15, 15, 16)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "y") + agg_line() + agg_ylim(10, 90, 5)
  print(p)
},
NA)

expect_error({
  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "xy") + agg_line() + agg_ylim(10, 90, 5) + agg_xlim(10, 100)
  print(p)
},
NA)

expect_error({
  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "x") + agg_line() + agg_xlim(10, 100)
  print(p)
},
NA)

expect_error({
  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "x") + agg_line()
  print(p)
},
"You must manually set x axis limits for log scale plots.")

expect_error({
  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "y") + agg_line()
  print(p)
},
"You must manually set y axis limits for log scale plots.")

graphics.off()

