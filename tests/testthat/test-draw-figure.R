context("Draw figure")
set.seed(42)
randomdata <- ts(data.frame(x1 = rnorm(12)), start = c(2000,1), frequency = 4)

test_that("Filetypes", {
  # test that each of the filenames output the right type of file
  for (suffix in c("png","pdf","emf","svg")) {
    file <- paste("foo.", suffix, sep = "")
    agg_qplot(randomdata, filename = file)
    # test exists
    expect_true(file.exists(file))
    # remove
    file.remove(file)
  }

  # Special case EMF+, because we change the file extension
  agg_qplot(randomdata, filename = "foo.emf+")
  expect_true(file.exists("foo.emf"))
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

  expect_error(print(arphitgg(layout = "foo")), "Unknown layout option foo. Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2.")
})

test_that("1h layout", {
  q <- arphitgg(data.frame(), agg_aes(x=letters[1:10],1:10), layout = "1h")+agg_col() + agg_point()
  expect_true(check_graph(q, "draw-figure-1h-basic"))

  p <-
    arphitgg(data.frame(
      x = c(
        "Some long ticks labels",
        "Yeah, really really really long stuff",
        "Last one, probably"
      ),
      y = 1:3
    ),
    agg_aes(x, y), layout = "1h") + agg_col()
  expect_true(check_graph(p, "draw-figure-1h-long-labels"))

  p <- q + agg_title("foo")+agg_subtitle("bar")+agg_footnote("baz")+agg_source("qux")
  expect_true(check_graph(p, "draw-figure-1h-titles-notes"))

  p <- q + agg_units("foobarbazbusquxz")
  expect_true(check_graph(p, "draw-figure-1h-yunits-padding"))

  p <- q + agg_xaxislabel("x axis label") + agg_yaxislabel("y axis label")
  expect_true(check_graph(p, "draw-figure-1h-axis-label"))

  p <- arphitgg(data.frame(), agg_aes(x=letters[1:10],1:10), layout = "1h", log_scale = "y") +
    agg_col() + agg_point() + agg_ylim(0.5, 10.5, 6)
  expect_true(check_graph(p, "draw-figure-1h-logy"))

  p <- arphitgg(data.frame(), agg_aes(x=letters[1:10],1:10), layout = "1h", log_scale = "x") +
    agg_point() + agg_xlim(1,10)
  expect_true(check_graph(p, "draw-figure-1h-logx"))

  p <- arphitgg(data.frame(), agg_aes(x=c(1.5,8,1.5,3),y = 2:5), layout = "1h", log_scale = "xy") +
    agg_point() + agg_ylim(1, 11, 6) + agg_xlim(1, 10)
  expect_true(check_graph(p, "draw-figure-1h-logxy"))
})
