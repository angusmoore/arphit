context("Animations")

foo <- data.frame(
  series = rep(1:4, each = 10),
  x = rep(seq.Date(from = as.Date("2001-01-01"),
                      by = "quarter",
                      length.out = 10)),
  y = c(-0.4, 1.1, 1.4, -0.1, 0.4, 0.7, 1.5, -0.2, 0.1, -0.2,
        -0.5, -0.9, -0.1, -0.2, -0.6, 1.3, 0.9, -0.5, 0.7, -1,
        -1.5, -6, -8.6, -6.6, -5, -12, -18, 17, 13, 5,
        -1.7, 1.5, -1.8, 2.4, -11.7, 10.1, 1.4, -4, -4.2, -4.8))

test_that("Simple animations", {
  foo_list <- split(foo, foo$series)

  gg_list <- lapply(foo_list, function(data) {
    gg <- arphitgg(data = data, aes = agg_aes(x = x, y = y, group = series)) +
      agg_line() + agg_ylim(-20, 20, 5)
  })

  gif_file <- agg_slides(gg_list, filename = tempfile("ani", fileext = ".gif"))
  expect_true(check_gif(gg_list, "animations-1"))
})
