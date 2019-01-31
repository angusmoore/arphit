context("Series attribute handling")
set.seed(42)
data <- data.frame(x=1:10,y=rnorm(10),y2=rnorm(10))

# Default attributes, one panel
test_that("Default attributes", {
  p <- arphitgg(data, agg_aes(x=x)) + agg_line(agg_aes(y=y)) + agg_line(agg_aes(y=y2))
  expect_true(check_graph(p, "attributes-default"))

})

test_that("Setting attributes", {
  # Set different attributes for each series
  p <- arphitgg(data, agg_aes(x=x)) +
    agg_line(agg_aes(y=y), color = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9) +
    agg_line(agg_aes(y=y2), color = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10)
  expect_true(check_graph(p, "attributes-set-all"))

  # Bar attributes
  p <- arphitgg(data, agg_aes(x=x,y=y)) +
    agg_col(color = "blue",
            barcol = "red")
  expect_true(check_graph(p, "attributes-bars"))

  # Set only one series attributes
  p <- arphitgg(data, agg_aes(x=x)) +
    agg_line(agg_aes(y=y), color = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9) +
    agg_line(agg_aes(y=y2))
  expect_true(check_graph(p, "attributes-set-one"))


  ## Two sided
  p <- arphitgg(data, agg_aes(x=x)) +
    agg_line(agg_aes(y=y), color = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9,
             panel = "1") +
    agg_line(agg_aes(y=y2), color = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10,
             panel = "2")
  expect_true(check_graph(p, "attributes-two-sided"))

})

test_that("Duplicate series names", {
  ## Duplicates
  p <- arphitgg(data, agg_aes(x=x)) +
    agg_line(agg_aes(y=y), color = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9,
             panel = "1") +
    agg_line(agg_aes(y=y), color = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10,
             panel = "2")
  expect_true(check_graph(p, "attributes-two-sided-duplicate"))

})

test_that("Setting user default colours", {
  # Setting user colours (#176, which changed to using R's options, instead of my earlier awful hack)
  options(arphit.user_colors = c(RBA["Red1"],RBA["Blue10"],RBA["Olive1"]))
  p <- arphitgg(data)+agg_line(agg_aes(x=x,y=y))+agg_line(agg_aes(x=x,y=y2))
  expect_true(check_graph(p, "attributes-changed-defaults"))

  # And change back
  options(arphit.user_colors = NULL)
  p <- arphitgg(data)+agg_line(agg_aes(x=x,y=y))+agg_line(agg_aes(x=x,y=y2))
  expect_true(check_graph(p, "attributes-reverted-defaults"))

})
