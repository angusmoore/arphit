set.seed(42)
data <- data.frame(x = 1:10, y = rnorm(10), y2 = rnorm(10))

# Default attributes, one panel
test_that("Default attributes", {
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = y)) +
    agg_line(agg_aes(y = y2))
  expect_true(check_graph(p, "attributes-default"))
})

test_that("Setting attributes", {
  # Set different attributes for each series
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = y),
             colour = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9) +
    agg_line(agg_aes(y = y2),
             colour = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10)
  expect_true(check_graph(p, "attributes-set-all"))

  # Bar attributes
  p <- arphitgg(data, agg_aes(x = x, y = y)) +
    agg_col(colour = "blue",
            barcol = "red")
  expect_true(check_graph(p, "attributes-bars"))

  # Set only one series attributes
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = y), colour = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9) +
    agg_line(agg_aes(y = y2))
  expect_true(check_graph(p, "attributes-set-one"))

  ## Two sided
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = y), colour = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9,
             panel = "1") +
    agg_line(agg_aes(y = y2), colour = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10,
             panel = "2")
  expect_true(check_graph(p, "attributes-two-sided"))

})

test_that("Duplicate series names", {
  ## Duplicates
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = y), colour = "red",
             pch = 1,
             lty = 3,
             lwd = 5,
             pointsize = 9,
             panel = "1") +
    agg_line(agg_aes(y = y), colour = "green",
             pch = 2,
             lty = 4,
             lwd = 6,
             pointsize = 10,
             panel = "2")
  expect_true(check_graph(p, "attributes-two-sided-duplicate"))
})

test_that("Setting user default colours", {
  # Setting user colours (#176)
  options(arphit.user_colours = c(RBA["Red1"], RBA["Blue10"], RBA["Olive1"]))
  p <- arphitgg(data) +
    agg_line(agg_aes(x = x, y = y)) +
    agg_line(agg_aes(x = x, y = y2))
  expect_true(check_graph(p, "attributes-changed-defaults"))

  # And change back
  options(arphit.user_colours = NULL)
  p <- arphitgg(data) +
    agg_line(agg_aes(x = x, y = y)) +
    agg_line(agg_aes(x = x, y = y2))
  expect_true(check_graph(p, "attributes-reverted-defaults"))
})

# Error handling in qplot
test_that("qplot attribute error handling", {
  expect_error(
    agg_qplot(data.frame(x = 1:10, y = 1:10),
              x = "x",
              col = list("foo" = "red")),
    "You have tried to set attributes for foo but it is not a series in your data." #nolint
  )
})
