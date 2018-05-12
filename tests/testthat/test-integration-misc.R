# #71 incorrect handling of NAs
bar <- data.frame(x = c(2000:2003), y = c(NA, rnorm(3)), z = c(NA, y = rnorm(3)))
expect_error(agg_qplot(bar, x="x"), NA)
expect_error(agg_qplot(bar, x="x", joined = FALSE), NA)

graphics.off()
