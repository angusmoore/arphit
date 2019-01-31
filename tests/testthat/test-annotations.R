context("Annotations")

## Labels ===================

# Labels
test_that("Labels", {
  foo <- arphitgg() + agg_label("Text", 2002, 0.2, "1", "red")
  bar <- arphitgg() + agg_label("Text", 2002, 0.2, "1", "red") +
    agg_label("Second label",2003, -0.2, "1", "green", 40)
  expect_true(check_graph(foo, "annotations-label1"))
  expect_true(check_graph(bar, "annotations-label2"))
})

## Arrows ==================

test_that("Arrows", {
  foo <- arphitgg() + agg_arrow(2000, 0, 2001, 2, "red", "1")
  bar <- arphitgg() + agg_arrow(2000, 0, 2001, 2, "red", "1") + agg_arrow(2001, 0, 2002, 2, "green", "1", 2)
  expect_true(check_graph(foo, "annotations-arrows1"))
  expect_true(check_graph(bar, "annotations-arrows2"))
})

## Background shading ================

test_that("Background shading", {
  foo <- arphitgg() + agg_bgshading(x1 = 2001, x2 = 2002, panel = "1")
  bar <- arphitgg() + agg_bgshading(y1 = 0.5, y2 = -0.5, panel = "1")
  baz <- arphitgg() + agg_bgshading(x1 = 2001, x2 = 2002, y1 = 0.5, y2 = -0.5, color = "red", panel = "1")

  expect_true(check_graph(foo, "annotations-bgshading1"))
  expect_true(check_graph(bar, "annotations-bgshading2"))
  expect_true(check_graph(baz, "annotations-bgshading3"))
})

## AB lines ================

test_that("AB lines", {
  foo <- arphitgg() + agg_abline(x = 2001, color = RBA["Blue1"], panel = "1")
  bar <- arphitgg() + agg_abline(y = -0.5, color = RBA["Red1"], panel = "1") +
    agg_abline(x = 2001, color = RBA["Blue1"], panel = "1")
  baz <- arphitgg() + agg_abline(x1 = 2000, y1 = -0.1, x2 = 2002, y2 = 0.5, panel = "1")
  p <- arphitgg() + agg_abline(x = 2002, lty = 5, lwd = 20, panel = "1")

  expect_true(check_graph(foo, "annotations-ablines1"))
  expect_true(check_graph(bar, "annotations-ablines2"))
  expect_true(check_graph(baz, "annotations-ablines3"))
  expect_true(check_graph(p, "annotations-set-lty-lwd"))
})

## Extrapolating on missing arguments for ab lines ============

test_that("Extrapolate ab lines arguments", {
  expect_equal(agg_abline(x=2001,panel="1",color="green",lty=2),
                 list(
                   x1 = 2001,
                   y1 = NA,
                   x2 = 2001,
                   y2 = NA,
                   color = "green",
                   panel = "1",
                   lwd=1,
                   lty=2,
                   type="abline"
                 )
               )

  expect_equal(agg_abline(y=-0.5,panel="1"),
    list(
      x1 = NA,
      y1 = -0.5,
      x2 = NA,
      y2 = -0.5,
      color = "black",
      panel = "1",
      lwd = 1,
      lty = 1,
      type = "abline"
    ))

  expect_equal(agg_abline(x1=2000,y1=-1,x2=2001,y2=0,panel='1'),
    list(
      x1 = 2000,
      y1 = -1,
      x2 = 2001,
      y2 = 0,
      color = "black",
      panel = "1",
      lwd = 1,
      lty = 1,
      type = "abline"
    ))
  nopanel <- list(list(x = 2001))
  specificerror <- list(list(
    x1 = 2000,
    y1 = -1,
    y2 = 0,
    panel = 1
  ))

  expect_error(agg_abline(x = 2001),
               "argument \"panel\" is missing, with no default")
  expect_error(
    agg_abline(
      x1 = 2000,
      y1 = -1,
      y2 = 0,
      panel = "1"
    ),
    "Line was specified without x or y (i.e. not a horizontal or vertical line), but is missing x2.",
    fixed = TRUE
  )
  expect_error(
    agg_abline(
      x1 = 2000,
      x2 = 2001,
      y1 = -1,
      panel = "1"
    ),
    "Line was specified without x or y (i.e. not a horizontal or vertical line), but is missing y2.",
    fixed = TRUE
  )
  expect_error(
    agg_abline(
      x2 = 2000,
      y1 = -1,
      y2 = 0,
      panel = "1"
    ),
    "Line was specified without x or y (i.e. not a horizontal or vertical line), but is missing x1.",
    fixed = TRUE
  )
  expect_error(
    agg_abline(
      x1 = 2000,
      x2 = 2001,
      y2 = -1,
      panel = "1"
    ),
    "Line was specified without x or y (i.e. not a horizontal or vertical line), but is missing y1.",
    fixed = TRUE
  )
})
