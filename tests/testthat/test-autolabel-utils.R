context("Autolabeller utils")

# Create labels
foo <- data.frame(x1=rnorm(10),x2=rnorm(10))
expect_equal(createlabels(foo, list("1" = "x1", "2" = "x2"), "1", "1"), list(x1 = "x1\n(LHS)", x2 = "x2\n(RHS)"))
expect_equal(createlabels(foo, list("1" = c("x1","x2")), "1", "1"), list(x1 = "x1", x2 = "x2"))

# Convert data for axes
# no op first
ylim_list <- list("1" = list(min = -1.5, max = 3, nsteps = 4), "2" = list(min = -1.5, max = 3, nsteps = 4))
bar <- data.frame(x=1:10,x1=rnorm(10),x2=rnorm(10))
foo <- list("1" = bar[, c("x","x1")], "2" = bar[, c("x","x2")])
panels <- list("1" = "x1", "2" = "x2")
expect_equal(convertdata.axes(foo, panels, "1", "1", ylim_list), bar)
# now double the y axis on the RHS
ylim_list <- list("1" = list(min = -1.5, max = 3, nsteps = 4), "2" = list(min = -3, max = 6, nsteps = 4))
bar_converted <- bar
bar_converted$x2 <- bar$x2 / 2
expect_equal(convertdata.axes(foo, panels, "1", "1", ylim_list), bar_converted)

# no op if no RHS data
no_RHS <- list("1" = c("x1", "x2"))
expect_equal(convertdata.axes(list("1" = bar), no_RHS, "1", "1", ylim_list), bar)

# no op on layouts without overlapping LHS/RHS
no_RHS <- list("1" = c("x1", "x2"), "2" = c("x1","x2"))
expect_equal(convertdata.axes(list("1" = bar), no_RHS, "1", "2v", ylim_list), bar)

# not already labelled
labels <- list(list(panel = "3"))
expect_true(notalreadylabelled("1", labels))
expect_false(notalreadylabelled("3", labels))

# Not RHS
expect_true(notRHS("1", "1"))
expect_true(notRHS("1", "2h"))
expect_false(notRHS("2", "2h"))
expect_true(notRHS("1", "4h"))
expect_false(notRHS("6", "4h"))
expect_true(notRHS("1", "3v"))
expect_true(notRHS("2", "3v"))
expect_true(notRHS("3", "3v"))

# Format labels
warning("No tests for format labels")
