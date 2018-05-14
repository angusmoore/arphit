context("Autolabeller")

# Create labels
foo <- data.frame(x1=rnorm(10),x2=rnorm(10))
expect_equal(createlabels(foo, list("1" = "x1", "2" = "x2"), "1", "1"), list(x1 = "x1\n(LHS)", x2 = "x2\n(RHS)"))
expect_equal(createlabels(foo, list("1" = c("x1","x2")), "1", "1"), list(x1 = "x1", x2 = "x2"))

# Convert data for axes
# no op first
ylim_list <- list("1" = list(min = -1.5, max = 3, nsteps = 4), "2" = list(min = -1.5, max = 3, nsteps = 4))
foo <- data.frame(x1=rnorm(10),x2=rnorm(10))
panels <- list("1" = "x1", "2" = "x2")
expect_equal(convertdata.axes(foo, panels, "1", "1", ylim_list), foo)
# now double the y axis on the RHS
ylim_list <- list("1" = list(min = -1.5, max = 3, nsteps = 4), "2" = list(min = -3, max = 6, nsteps = 4))
foo_converted <- foo
foo_converted$x2 <- foo$x2 / 2
expect_equal(convertdata.axes(foo, panels, "1", "1", ylim_list), foo_converted)

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
