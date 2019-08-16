## Legend ==============

data <- data.frame(x = 1:10, a = 1:10, b = 11:20, c = 21:30, d = 31:40)

test_that("Legend series and colours", {
  # basic legend tests over one panel, with left and right axes
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a), panel = "1", colour = "red") +
    agg_line(agg_aes(y = b), panel = "1", colour = "green") +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-basic-legend"))

  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a), panel = "1", colour = "red") +
    agg_line(agg_aes(y = b), panel = "1", colour = "green") +
    agg_line(agg_aes(y = c), panel = "2", colour = "blue") +
    agg_line(agg_aes(y = d), panel = "2", colour = "yellow") +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-basic-legend-lhs-rhs"))

  # check pch works too
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a), panel = "1", colour = "red", pch = 20) +
    agg_line(agg_aes(y = b), panel = "1", colour = "green") +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-pch"))
   
  # With an axis label too
  p <- p + agg_xaxislabel("Fizz Buzz")
  expect_true(check_graph(p, "draw-outer-legend-with-axislabel"))
  
  # tests for duplicate series names
  # same attributes for duplicate
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a), panel = "1", colour = "red") +
    agg_line(agg_aes(y = b), panel = "1", colour = "green") +
    agg_line(agg_aes(y = a), panel = "2", colour = "red") +
    agg_line(agg_aes(y = d), panel = "2", colour = "yellow") +
    agg_legend()
  expect_true(check_graph(p,
                          "draw-outer-basic-duplicate-names-same-attributes"))

  # different attributes for duplicate
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a), panel = "1", colour = "red") +
    agg_line(agg_aes(y = b), panel = "1", colour = "green") +
    agg_line(agg_aes(y = a), panel = "2", colour = "blue") +
    agg_line(agg_aes(y = d), panel = "2", colour = "yellow") +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-basic-duplicate-names"))
})

# determining legend size
test_that("Legend columns", {
  data <- data.frame(x = 1:10, foo = 1:10, bar = 1:10, baz = 1:10)
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = foo)) +
    agg_line(agg_aes(y = bar)) +
    agg_line(agg_aes(y = baz)) +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-legend-1-row"))

  data <- data.frame(x = 1:10,
                     aaaaaaaaaaaaaaaaaaaaaaaaaa1 = 1:10,
                     aaaaaaaaaaaaaaaaaaaaaaaaaa2 = 1:10,
                     aaaaaaaaaaaaaaaaaaaaaaaaaa3 = 1:10)
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = aaaaaaaaaaaaaaaaaaaaaaaaaa1)) +
    agg_line(agg_aes(y = aaaaaaaaaaaaaaaaaaaaaaaaaa2)) +
    agg_line(agg_aes(y = aaaaaaaaaaaaaaaaaaaaaaaaaa3)) +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-legend-3-row"))

  # manually set ncol
  data  <- data.frame(unemployment = 1:20,
                      employment = 1:20,
                      state = c(rep("A", 5),
                                rep("B", 5),
                                rep("C", 5),
                                rep("D", 5)),
                      date = seq.Date(from = as.Date("2017-01-10"),
                                      length.out = 10,
                                      by = "quarter"))
  p <- arphitgg(data) +
    agg_line(agg_aes(x = date, y = unemployment, group = state)) +
    agg_line(agg_aes(x = date, y = employment, group = state), pch = 20) +
    agg_legend(ncol = 3)
  expect_true(check_graph(p, "draw-outer-manual-ncol"))
})

test_that("On panel legend", {
  data  <- data.frame(unemployment = 1:20,
                      employment = 1:20,
                      state = c(rep("A", 5),
                                rep("B", 5),
                                rep("C", 5),
                                rep("D", 5)),
                      date = seq.Date(from = as.Date("2017-01-10"),
                                      length.out = 10,
                                      by = "quarter"))
  p <- arphitgg(data) +
    agg_line(agg_aes(x = date, y = unemployment, group = state)) +
    agg_legend(x = "topleft")
  expect_true(check_graph(p, "draw-outer-onpanel-legend"))

  p <- arphitgg(data) +
    agg_line(agg_aes(x = date, y = unemployment, group = state)) +
    agg_legend(x = 0.5, y = 0.5)
  expect_true(check_graph(p, "draw-outer-onpanel-legend-manual-location"))

  ## Special case handling of when we have too few legend names to spill into
  # the last legend column, so placement looks weird (#305)
  data <- data.frame(x = rep(1:10, 4),
                     y = 1:40,
                     group = sort(rep(c("Foobar",
                                        "Foobar32",
                                        "Fuzzbuzz",
                                        "Baz"),
                                      10),
                                  na.last = TRUE))

  p <- arphitgg(data, agg_aes(x = x, y = y, group = group)) +
    agg_col() + agg_legend(x = "topright")
  expect_true(check_graph(p, "draw-outer-onpanel-legend-exact-columns-305"))

  ## Errors
  expect_error(
    arphitgg() + agg_legend(x = 1),
    "You must specify a y coordinate"
  )

  expect_error(
    arphitgg() + agg_legend(x = "f"),
    "Valid options for automatic placement of on panel legend are"
  )

})

# Don't include series wihtout names in legends/autolabeller (#219)
test_that("Miscellaneous", {
  data <- tibble::tibble(x = 1:10,
                         a = 1:10,
                         b = 11:20,
                         `<NA>` = 21:30)
  p <- arphitgg(data, agg_aes(x = x)) +
    agg_line(agg_aes(y = a)) +
    agg_line(agg_aes(y = b)) +
    agg_line(agg_aes(y = `<NA>`)) +
    agg_legend()
  expect_true(check_graph(p, "draw-outer-no-na"))
})
