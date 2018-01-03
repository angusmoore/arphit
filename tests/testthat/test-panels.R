# Set up
fakeseries1 <- c("a", "b")
fakeseries2 <- c("c", "d")
fakeseries3 <- c("e", "f")
fakeseries4 <- c("g", "h")
nobars <- NULL
overlapseries1 <- c("a", "e")

context("Panel handling")
# Not supplying a list, defaulting
shouldbe <- list(panels = list("1" = fakeseries1, "2" = NULL), serieslist = list("a" = "1", "b" = "1"), bars = list())
expect_that(handlepanels(fakeseries1, nobars, "1"), is_identical_to(shouldbe))

# Same, but supplying just the LHS
expect_that(handlepanels(list("1" = fakeseries1, "2" = NULL), nobars, "1"), is_identical_to(shouldbe))

# One panel, both sides
shouldbe <- list(panels = list("1" = fakeseries1, "2" = fakeseries2), serieslist = list("a" = "1", "b" = "1", "c" = "2", "d" = "2"), bars = list())
expect_that(handlepanels(list("1" = fakeseries1, "2" = fakeseries2), nobars, "1"), is_identical_to(shouldbe))

# Two panel horizontal
shouldbe <- list(panels = list("1" = fakeseries1, "2" = NULL, "3" = fakeseries2, "4" = NULL), serieslist = list("a" = "1", "b" = "1", "c" = "3", "d" = "3"), bars = list())
expect_that(handlepanels(list("1" = fakeseries1, "3" = fakeseries2), nobars, "2h"), is_identical_to(shouldbe))

# Two panel vertical
shouldbe <- list(panels = list("1" = fakeseries1, "2" = fakeseries2), serieslist = list("a" = "1", "b" = "1", "c" = "2", "d" = "2"), bars = list())
expect_that(handlepanels(list("1" = fakeseries1, "2" = fakeseries2), nobars, "2v"), is_identical_to(shouldbe))

# 2b2
shouldbe <- list(panels = list("1" = fakeseries1, "2" = fakeseries2, "3" = fakeseries3, "4" = fakeseries4), serieslist = list("a" = "1", "b" = "1", "c" = "2", "d" = "2", "e" = "3", "f" = "3", "g" = "4", "h" = "4"), bars = list())
expect_that(handlepanels(list("1" = fakeseries1, "2" = fakeseries2, "3" = fakeseries3, "4" = fakeseries4), nobars, "2b2"), is_identical_to(shouldbe))


context("Panel handling - Bars")
shouldbe <- list(panels = list("1" = fakeseries1, "2" = NULL), serieslist = list("a" = "1", "b" = "1"), bars = list("a" = TRUE, "b" = TRUE))
expect_that(handlepanels(fakeseries1, fakeseries1, "1"), is_identical_to(shouldbe))

shouldbe <- list(panels = list("1" = fakeseries1, "2" = fakeseries2), serieslist = list("a" = "1", "b" = "1", "c" = "2", "d" = "2"), bars = list("a" = TRUE, "b" = TRUE))
expect_that(handlepanels(list("1" = fakeseries1, "2" = fakeseries2), fakeseries1, "1"), is_identical_to(shouldbe))

# Handle panels - errors
context("Panel handling - errors")
expect_error(handlepanels(list("1" = fakeseries1, "3" = fakeseries3), nobars, "1"))
expect_error(handlepanels(list("1" = fakeseries1, "3" = fakeseries3), nobars, "2v"))
expect_error(handlepanels(list("1" = fakeseries1, "5" = fakeseries3), nobars, "2h"))
expect_error(handlepanels(list("1" = fakeseries1, "5" = fakeseries3), nobars, "2b2"))
expect_error(handlepanels(list("1" = fakeseries1, "1" = fakeseries3), nobars, "foo"))
