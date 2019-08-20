library(dplyr)

## Log scales =====================
test_that("Log scales", {
  # Log scales (#161)
  p <- data.frame(x = c(1.5, 2, 3, 4, 4.5, 5, 6, 7, 8, 9),
                  y = c(11, 20, 40, 90, 11, 14, 90, 15, 15, 16)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "xy") + agg_line() +
    agg_ylim(10, 90, 5) + agg_xlim(1, 10)
  expect_true(check_graph(p, "misc-log-scale-both"))

  p <- data.frame(x = 1:10,
                  y = c(11, 20, 40, 90, 11, 14, 90, 15, 15, 16)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "y") +
    agg_line() + agg_ylim(10, 90, 5)
  expect_true(check_graph(p, "misc-log-scale-y"))

  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "xy") + agg_line() +
    agg_ylim(10, 90, 5) + agg_xlim(10, 100)
  expect_true(check_graph(p, "misc-log-scale-both-larger-scale"))

  p <- data.frame(x = c(10, 100, 60), y = c(11, 20, 40)) %>%
    arphitgg(agg_aes(x = x, y = y), log_scale = "x") + agg_line() +
    agg_xlim(10, 100)
  expect_true(check_graph(p, "misc-log-scale-x"))

  p <- data.frame(x = c(1, 2, 3), y = c(-1, 2, 3)) %>%
    arphitgg(agg_aes(x, y), log_scale = "y") + agg_col() + agg_ylim(1, 2, 3)
  expect_error(print(p), "y log scale plots cannot have negative data")
})

test_that("Log scale limit requirement", {
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
})

## joined (#101) ==================

test_that("Joined", {
  foo <- data.frame(x = 1:10, y = 1:10)
  foo$y[4] <- NA
  p <- arphitgg(foo, agg_aes(x = x, y = y), joined = FALSE) + agg_line()
  expect_true(check_graph(p, "misc-joined"))
})

## SRT ================

test_that("srt", {
  foo <- data.frame(x = c("a very long label", "another very long label"),
                    y = c(1, 2))
  p <- arphitgg(foo, agg_aes(x = x, y = y), srt = 90) + agg_col()
  expect_true(check_graph(p, "misc-long-rotated-x-labels-90"))

  p <- arphitgg(foo, agg_aes(x = x, y = y), srt = 45) + agg_col()
  expect_true(check_graph(p, "misc-long-rotated-x-labels-45"))
})
