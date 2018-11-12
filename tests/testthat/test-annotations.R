context("Annotations")

# Extrapolating on missing arguments for ab lines
vline <- list(list(
  x = 2001,
  panel = 1,
  color = "green",
  lty = 2
))
twovline <-
  list(
    list(
      x = 2001,
      panel = 1,
      color = "green",
      lty = 2
    ),
    list(
      x = 2002,
      panel = 1,
      color = "red",
      lty = 3
    )
  )
hline <- list(list(y = -0.5, panel = 1))
specificline <-
  list(list(
    x1 = 2000,
    y1 = -1,
    x2 = 2001,
    y2 = 0,
    panel = 1
  ))
nopanel <- list(list(x = 2001))
specificerror <- list(list(
  x1 = 2000,
  y1 = -1,
  y2 = 0,
  panel = 1
))

expect_that(sanitychecklines(vline), is_identical_to(list(
  list(
    x = 2001,
    panel = 1,
    color = "green",
    lty = 2,
    x1 = 2001,
    x2 = 2001,
    y1 = NA,
    y2 = NA
  )
)))
expect_that(sanitychecklines(hline), is_identical_to(list(
  list(
    y = -0.5,
    panel = 1,
    x1 = NA,
    x2 = NA,
    y1 = -0.5,
    y2 = -0.5,
    color = "black",
    lty = 1
  )
)))
expect_that(sanitychecklines(specificline), is_identical_to(list(
  list(
    x1 = 2000,
    y1 = -1,
    x2 = 2001,
    y2 = 0,
    panel = 1,
    color = "black",
    lty = 1
  )
)))
expect_that(sanitychecklines(twovline), is_identical_to(list(
  list(
    x = 2001,
    panel = 1,
    color = "green",
    lty = 2,
    x1 = 2001,
    x2 = 2001,
    y1 = NA,
    y2 = NA
  ),
  list(
    x = 2002,
    panel = 1,
    color = "red",
    lty = 3,
    x1 = 2002,
    x2 = 2002,
    y1 = NA,
    y2 = NA
  )
)))
expect_error(sanitychecklines(nopanel))
expect_error(sanitychecklines(specificerror))
