context("Deprecation warnings")

test_that("Deprecation warnings for color", {
  data <- data.frame(x=1:10,y=1:10)
  expect_warning(
    arphitgg(data) + agg_line(agg_aes(x=x,y=y),color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg(data) + agg_col(agg_aes(x=x,y=y),color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg(data) + agg_point(agg_aes(x=x,y=y),color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg() + agg_label(x=1,y=2,text="a",panel="1",color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg() + agg_arrow(tail.x=1,tail.y=2,head.x=1,head.y=3,panel="1",color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg() + agg_abline(x=4,panel="1",color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  expect_warning(
    arphitgg() + agg_bgshading(x1=1,y1=2,x2=2,y2=3,panel="1",color="red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )

  data <- data.frame(x=1:10,y=2:11,z=3:12)
  expect_warning(
    arphitgg(data) + agg_line(agg_aes(x=x,y=y)) + agg_line(agg_aes(x=x,y=z)) + agg_shading(from = z, to = y, color = "red"),
    "color is deprecated; use colour instead",
    fixed = TRUE
  )
})

test_that("Deprecations for agg_vline and hline", {
  expect_warning(
    arphitgg() + agg_abline(x = 1, panel = "1"),
    "x to draw a vertical line is deprecated. Use `agg_vline`"
  )

  expect_warning(
    arphitgg() + agg_abline(y = 1, panel = "1"),
    "y to draw a horizontal line is deprecated. Use `agg_hline`"
  )
})
