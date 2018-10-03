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

graphics.off()

