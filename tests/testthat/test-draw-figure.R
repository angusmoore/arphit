context("Draw figure")
set.seed(42)
randomdata <- ts(data.frame(x1 = rnorm(12)), start = c(2000,1), frequency = 4)

test_that("Filetypes", {
  # test that each of the filenames output the right type of file
  for (suffix in c("png","pdf","emf","svg")) {
    file <- paste("foo.", suffix, sep = "")
    agg_qplot(randomdata, filename = file)
    # test exists
    expect_that(file.exists(file), is_true())
    # remove
    file.remove(file)
  }

  # Special case EMF+, because we change the file extension
  agg_qplot(randomdata, filename = "foo.emf+")
  expect_that(file.exists("foo.emf"), is_true())
  file.remove("foo.emf")

  p <- arphitgg()
  expect_error(agg_draw(p, "foo.foo"), "Unsupported file type foo.")
})

test_that("Margins", {
  # tests for bottom spacing
  p <- arphitgg() + agg_xaxislabel("FOO")
  expect_true(check_graph(p, "draw-figure-with-extra-bottom-spacing"))

  # tests for extra margins when have y axis labels
  p <- arphitgg() + agg_yaxislabel("FOO")
  expect_true(check_graph(p, "draw-figure-with-extra-left-spacing"))

  # tests for extra margins with legends
  p <- arphitgg(randomdata, agg_aes(y=x1))+agg_line()+agg_point()+agg_legend()
  expect_true(check_graph(p, "draw-figure-legend-spacing"))

  # rotated x labels should have larger bottom padding
  p <- arphitgg(randomdata, agg_aes(y=x1), srt = 45)+agg_line()
  expect_true(check_graph(p, "draw-figure-srt-45"))
  p <- arphitgg(randomdata, agg_aes(y=x1), srt = 90)+agg_line()
  expect_true(check_graph(p, "draw-figure-srt-90"))
})

## Manual plotsize ================

test_that("Plotsize", {
  foo <- data.frame(x=1:10, y = 1:10)
  p <- arphitgg(foo, agg_aes(x=x,y=y), plotsize=c(2,5)) + agg_line()
  expect_true(check_graph(p, "draw-figure-manual-plotsize"))

  p <- arphitgg(foo, agg_aes(x=x,y=y), portrait = TRUE) + agg_line()
  expect_true(check_graph(p, "draw-figure-portrait"))
})

test_that("Layouts", {
  for (layout in c("1","2h","2v","3h","3v","3b2","4h","4b2")) {
    p <- arphitgg(layout = layout)
    expect_true(check_graph(p, paste0("draw-figure-", layout)))
  }

  expect_error(print(arphitgg(layout = "foo")), "Unknown layout option foo. Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2.")
})
