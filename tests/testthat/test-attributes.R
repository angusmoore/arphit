context("Series attribute handling")

onesided <- handlepanels(list("1" = c("x1","x2")), "1")
twosided <- handlepanels(list("1" = "x1", "2" = "x2"), "1")
duplicate <- handlepanels(list("1" = "x1", "2" = "x1"), "1")

# Default attributes for everyone
shouldbe <- list("1" = list(col = list("x1" = unname(RBA["Aqua8"]),
                                       "x2" = unname(RBA["Orange2"])),
                            pch = list("x1" = NA,
                                       "x2" = NA),
                            lty = list("x1" = 1,
                                       "x2" = 1),
                            lwd = list("x1" = 2,
                                       "x2" = 2),
                            barcol = list("x1" = NA,
                                          "x2" = NA),
                            pointsize = list("x1" = 1,
                                             "x2" = 1)),
                 "2" = list(col = list(),
                            pch = list(),
                            lty = list(),
                            lwd = list(),
                            barcol = list(),
                            pointsize = list()))
expect_that(handleattributes(onesided, NULL, NULL, NULL, NULL, NULL, NULL), equals(shouldbe))

# Set different attributes for each series
shouldbe <- list("1" = list(col = list("x1" = "red",
                                       "x2" = "green"),
                            pch = list("x1" = 1,
                                       "x2" = 2),
                            lty = list("x1" = 3,
                                       "x2" = 4),
                            lwd = list("x1" = 5,
                                       "x2" = 6),
                            barcol = list("x1" = 7,
                                          "x2" = 8),
                            pointsize = list(x1 = 9,
                                             x2 = 10)),
                 "2" = list(col = list(),
                            pch = list(),
                            lty = list(),
                            lwd = list(),
                            barcol = list(),
                            pointsize = list()))
# Without panel annotations
expect_that(handleattributes(
  onesided,
  list(x1 = "red", x2 = "green"),
  list(x1 = 1, x2 = 2),
  list(x1 = 3, x2 = 4),
  list(x1 = 5, x2 = 6),
  list(x1 = 7, x2 = 8),
  list(x1 = 9, x2 = 10)
),
equals(shouldbe))
# With panel annotations
expect_that(handleattributes(
  onesided,
  list("1" = list(x1 = "red", x2 = "green")),
  list("1" = list(x1 = 1, x2 = 2)),
  list("1" = list(x1 = 3, x2 = 4)),
  list("1" = list(x1 = 5, x2 = 6)),
  list("1" = list(x1 = 7, x2 = 8)),
  list("1" = list(x1 = 9, x2 = 10))
),
equals(shouldbe))

# Set only one series attributes
shouldbe <- list("1" = list(col = list(x1 = "red",
                                       x2 = unname(RBA["Aqua8"])),
                            pch = list(x1 = 100,
                                       x2 = NA),
                            lty = list(x1 = 200,
                                       x2 = 1),
                            lwd = list(x1 = 300,
                                       x2 = 2),
                            barcol = list(x1 = 400,
                                          x2 = NA),
                            pointsize = list(x1 = 500,
                                             x2 = 1)),

                "2" = list(col = list(),
                          pch = list(),
                          lty = list(),
                          lwd = list(),
                          barcol = list(),
                          pointsize = list()))
expect_that(handleattributes(
  onesided,
  list("x1" = "red"),
  list(x1 = 100),
  list("x1" = 200),
  list(x1 = 300),
  list(x1 = 400),
  list(x1 = 500)
),
equals(shouldbe))

expect_that(handleattributes(
  onesided,
  list("1" = list("x1" = "red")),
  list("1" = list(x1 = 100)),
  list("1" = list("x1" = 200)),
  list("1" = list(x1 = 300)),
  list("1" = list(x1 = 400)),
  list("1" = list(x1 = 500))
),
equals(shouldbe))

## Two sided
shouldbe <- list("1" = list(col = list("x1" = "red"),
                            pch = list("x1" = 1),
                            lty = list("x1" = 3),
                            lwd = list("x1" = 5),
                            barcol = list("x1" = 7),
                            pointsize = list(x1 = 9)),
                 "2" = list(col = list("x2" = "green"),
                            pch = list("x2" = 2),
                            lty = list("x2" = 4),
                            lwd = list("x2" = 6),
                            barcol = list("x2" = 8),
                            pointsize = list(x2 = 10)))

# Without panel annotations
expect_that(handleattributes(
  twosided,
  list(x1 = "red", x2 = "green"),
  list(x1 = 1, x2 = 2),
  list(x1 = 3, x2 = 4),
  list(x1 = 5, x2 = 6),
  list(x1 = 7, x2 = 8),
  list(x1 = 9, x2 = 10)
),
equals(shouldbe))
# With panel annotations
expect_that(handleattributes(
  twosided,
  list("1" = list(x1 = "red"), "2" = list(x2 = "green")),
  list("1" = list(x1 = 1), "2" = list(x2 = 2)),
  list("1" = list(x1 = 3), "2" = list(x2 = 4)),
  list("1" = list(x1 = 5), "2" = list(x2 = 6)),
  list("1" = list(x1 = 7), "2" = list(x2 = 8)),
  list("1" = list(x1 = 9), "2" = list(x2 = 10))
),
equals(shouldbe))

## Duplicates

# apply to both
shouldbe <- list("1" = list(col = list("x1" = "red"),
                            pch = list("x1" = 1),
                            lty = list("x1" = 2),
                            lwd = list("x1" = 3),
                            barcol = list("x1" = 4),
                            pointsize = list(x1 = 5)),
                 "2" = list(col = list("x1" = "red"),
                            pch = list("x1" = 1),
                            lty = list("x1" = 2),
                            lwd = list("x1" = 3),
                            barcol = list("x1" = 4),
                            pointsize = list(x1 = 5)))
expect_that(handleattributes(
  duplicate,
  list(x1 = "red"),
  list(x1 = 1),
  list(x1 = 2),
  list(x1 = 3),
  list(x1 = 4),
  list(x1 = 5)
),
equals(shouldbe))


# Give different in different panels
shouldbe <- list("1" = list(col = list("x1" = "red"),
                            pch = list("x1" = 1),
                            lty = list("x1" = 2),
                            lwd = list("x1" = 3),
                            barcol = list("x1" = 4),
                            pointsize = list(x1 = 5)),
                 "2" = list(col = list("x1" = "green"),
                            pch = list("x1" = 5),
                            lty = list("x1" = 6),
                            lwd = list("x1" = 7),
                            barcol = list("x1" = 8),
                            pointsize = list(x1 = 10)))

expect_that(handleattributes(
  duplicate,
  list("1" = list(x1 = "red"), "2" = list(x1 = "green")),
  list("1" = list(x1 = 1), "2" = list(x1 = 5)),
  list("1" = list(x1 = 2), "2" = list(x1 = 6)),
  list("1" = list(x1 = 3), "2" = list(x1 = 7)),
  list("1" = list(x1 = 4), "2" = list(x1 = 8)),
  list("1" = list(x1 = 5), "2" = list(x1 = 10))
),
equals(shouldbe))
