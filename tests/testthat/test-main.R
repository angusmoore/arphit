context("Integration tests")

data <- ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)

# These are just all the examples from the vignette, which should cover all the plotting options

# Panels and layouts section
expect_error(arphit(data, layout = "1", series = list("1" = c("x1","x4"))), NA)
expect_error(arphit(data, layout = "1", series = list("1" = c("x1"), "2" = c("x4"))), NA)
expect_error(arphit(data, layout = "2v", series = list("1" = c("x1"), "2" = c("x2"))), NA)
expect_error(arphit(data, layout = "2h", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4"))), NA)
expect_error(arphit(data, layout = "2h", series = list("1" = c("x1","x2"), "3" = c("x3","x4"))), NA)
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4"))), NA)
expect_error(arphit(data), NA)

# portrait size
expect_error(arphit(data, portrait = TRUE), NA)

# Bars
expect_error(arphit(data, series = list("1" = c("x1","x2"), "2" = c("x3")), bars = c("x2")), NA)
expect_error(arphit(data, series = list("1" = c("x1","x2")), bars = c("x1","x2")), NA)
expect_error(arphit(data, bars = c("x1","x2"), bar.stacked = FALSE), NA)

# Titles
expect_error(arphit(data, title = "Here is a Title", subtitle = "And a subtitle"), NA)

# Panel titles
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), paneltitle = list("1" = "Panel 1", "2" = "Panel 2", "3" = "Panel 3", "4" = "Panel 4")), NA)

# Footnotes and sources
expect_error(arphit(data, footnote = c("Footnote 1", "Footnote 2", "etc, this can go on for a while")), NA)
expect_error(arphit(data, sources = c("RBA", "ABS", "someone else")), NA)

# Scale units
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), scaleunits = list("1" = "index", "2" = "ppt", "3" = "$", "4" = "000s")), NA)

# Line, bar and marker options
expect_error(arphit(data, col = list("x2" = "pink", "x4" = "lightblue")), NA)
expect_error(arphit(data, pch = 19), NA)

# Axis limits
expect_error(arphit(data, series = list("1" = c("x1"), "2" = c("x3")), ylim = list("1" = list(min = -10, max = 10, nsteps = 5), "2" = list(min = -20, max = 20, nsteps = 5))), NA)
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), ylim = list(min = -10, max = 10, nsteps = 5)), NA)

# Shading between series
expect_error(arphit(data, shading = list(list(from = "x1", to = "x2", color= "red"))), NA)
expect_error(arphit(data, shading = list(list(from = "x1", to = "x2", color= "red"), list(from = "x3", to = "x4", color = "purple"))), NA)

# Plot annotations
expect_error(arphit(data, labels = list(list(x = 2001, y = 2, text = "A label", panel = 1, color = "red"), list(x = 2004, y = 10, text = "A label\n(On two lines!)", panel = 1, color = "green"))), NA)
expect_error(arphit(data, arrows = list(list(tail.x = 2000, tail.y = 10, head.x = 2001, head.y = 1, color = "darkred", panel = 1))), NA)
expect_error(arphit(data, lines = list(list(x = 2001, panel = 1), list(y = -1, color = "darkred", panel = 1, lty = 2))), NA)
expect_error(arphit(data, lines = list(list(x1 = 2000, y1 = -10, x2 = 2002, y2 = 5, panel = 1))), NA)
expect_error(arphit(data, bgshading = list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 3, panel = 1))), NA)
expect_error(arphit(data, layout = "2h", series = list("1" = "x4", "3" = "x2"), bgshading = list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 3, panel = 1), list(x1 = 2000.5, y1 = NA, x2 = 2001.5, y2 = NA, panel = 3, color = "lightgreen"))), NA)

graphics.off()
