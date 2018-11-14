context("Annotations")

# Extrapolating on missing arguments for ab lines
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

expect_error(agg_abline(x=2001), "argument \"panel\" is missing, with no default")
expect_error(agg_abline(x1=2000,y1=-1,y2=0,panel="1"), "Line was specified without x or y (i.e. not a horizontal or vertical line), but is missing x2.", fixed=TRUE)
