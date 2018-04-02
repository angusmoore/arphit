context("Integration tests")

data <- ts(data.frame(x1 = rnorm(12), x2 = rnorm(12), x3 = rnorm(12, sd = 10), x4 = rnorm(12, sd = 5)), start = c(2000,1), frequency = 4)
simple_data <- data.frame(date = seq.Date(from = as.Date("2017-01-10"), length.out = 5, by = "quarter"), y1 = rnorm(5))
long_data <- data.frame(date = rep(seq.Date(from = as.Date("2017-01-10"), length.out = 5, by = "quarter"), 2), y1 = rnorm(10), y2 = rnorm(10), group_var = c(rep("A", 5), rep("B", 5)))

# These are just all the examples from the vignette, which should cover all the plotting options

## Plotting options vignette

# Data section (covers categorical data)
mytsdata <- ts(data.frame(x1 = rnorm(10)), frequency = 4, start = c(2000, 1))
mydataframe <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 10, by = "quarter"), x1 = rnorm(10))
mytibble <- tibble::as_tibble(data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 10, by = "quarter"), x1 = rnorm(10)))
categoricaldata <- data.frame(categoryname = letters[1:5], value = rnorm(5))
scatterdata <- data.frame(x = rnorm(20), y = rnorm(20))
data1 <- data.frame(date = seq.Date(from = as.Date("2000-01-01"), length.out = 10, by = "quarter"), x1 = rnorm(10))
data2 <- data.frame(years = seq.Date(from = as.Date("2000-01-01"), length.out = 4, by = "year"), x2 = rnorm(4))

expect_error(arphit(mytsdata), NA)
expect_error(arphit(mydataframe, x = "date"), NA)
expect_error(arphit(mytibble, x = "date"), NA)
expect_error(arphit(categoricaldata, x = "categoryname"), NA)
expect_error(arphit(scatterdata, x = "x"), NA)
expect_error(arphit(data = list("1" = data1, "2" = data2), x = list("1" = "date", "2" = "years"), layout = "2v"), NA)

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

# Y units
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), yunits = list("1" = "index", "2" = "ppt", "3" = "$", "4" = "000s")), NA)

# Line, bar and marker options
expect_error(arphit(data, col = list("x2" = "pink", "x4" = "lightblue")), NA)
expect_error(arphit(data, pch = 19), NA)

# Axis limits
expect_error(arphit(data, series = list("1" = c("x1"), "2" = c("x3")), ylim = list("1" = list(min = -10, max = 10, nsteps = 5), "2" = list(min = -20, max = 20, nsteps = 5))), NA)
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), ylim = list(min = -10, max = 10, nsteps = 5)), NA)

# Drop x label
expect_error(arphit(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")), dropxlabel = TRUE), NA)

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
expect_error(arphit(data, layout = "2b2", yaxislabels = "A y axis label", xaxislabels = "An x label"), NA)
expect_error(arphit(data, layout = "2b2", yaxislabels = list("1" = "Foo", "3" = "Bar"), xaxislabels = list("3" = "An x label", "4" = "Another x label")), NA)

## EXTRA integration tests
# Numeric categorical labels
catdata1 <- data.frame(x = 2001:2005, y = 1:5)
expect_error(arphit(catdata1, x = "x"), NA)
# Non-one spaced numerical categorical labels
catdata2 <- data.frame(x = c(2,4,6,8,10), y = 1:5)
expect_error(arphit(catdata2, x = "x"), NA)

graphics.off()

## gg-interface vignette
p <- arphitgg(layout = "1")
expect_error(agg_draw(p), NA)

p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y1))
expect_error(agg_draw(p), NA)

p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var))
expect_error(agg_draw(p), NA)

p <- arphitgg(layout = "2h") +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var), panel = "1") +
  agg_col(data = long_data, aes = agg_aes(x = date, y = y2, group = group_var), panel = "3")
expect_error(agg_draw(p), NA)

p <- arphitgg(long_data, aes = agg_aes(x = date, group = group_var), layout = "2h") +
  agg_line(aes = agg_aes(y = y1), panel = "1") +
  agg_col(aes = agg_aes(y = y2), panel = "3")
expect_error(agg_draw(p), NA)

p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y1), color = RBA["Red1"])
expect_error(agg_draw(p), NA)

p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var), color = RBA["Red1"])
expect_error(agg_draw(p), NA)

p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var), color = c(RBA["Red1"], RBA["Blue4"]))
expect_error(agg_draw(p), NA)

p <- arphitgg() + agg_title("Graph Title Goes Here") + agg_subtitle("Or a subtitle, if you like")
expect_error(agg_draw(p), NA)

p <- arphitgg() + agg_title("Graph Title Goes Here") + agg_subtitle("Or a subtitle, if you like") +
  agg_title("And here is panel title", panel = "1") + agg_subtitle("Panel subtitle too if needed", panel = "1")
expect_error(agg_draw(p), NA)

p <- arphitgg() + agg_units("index")
expect_error(agg_draw(p), NA)

p <- arphitgg() + agg_units("index", panel = "1") + agg_units("ppt", panel = "2")
expect_error(agg_draw(p), NA)

p <- arphitgg() + agg_source("Source 1") + agg_source(c("Source 2 (as a vector)", "Source 3 (vectors are easy!")) +
  agg_footnote("This is my first footnoote") + agg_footnote("This is a second footnote")
expect_error(agg_draw(p), NA)

p <- arphitgg(simple_data, aes = agg_aes(x = date, y = y1))
expect_error(agg_draw(p, filename = "my-graph.png"), NA)
