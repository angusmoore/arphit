context("Integration - getting started")

simple_data <- data.frame(date = seq.Date(from = as.Date("2000-03-01"),
                                          length.out = 10,
                                          by = "quarter"),
                          y = rnorm(10))
long_data <- data.frame(date = rep(seq.Date(from = as.Date("2000-03-01"),
                                            length.out = 10,
                                            by = "quarter"), 2),
                        y = rnorm(20),
                        y2 = rnorm(20),
                        state = c(rep("A", 10), rep("B", 10)))
scatter_data <- data.frame(x = rnorm(10), y = rnorm(10))

# LAYOUTS PANELS AND TYPES OF GRAPHS =================

graphics.off()
p <- arphitgg(layout = "1")
print(p)
p <- arphitgg(layout = "2h")
print(p)

# CREATING A GRAPH ================

graphics.off()
p <- arphitgg(layout = "1")
print(p)
p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y))
print(p)
p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y, group = state))
print(p)
p <- arphitgg(layout = "2h") +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y, group = state), panel = "1") +
  agg_col(data = long_data, aes = agg_aes(x = date, y = y, group = state), panel = "3")
print(p)
p <- arphitgg(layout = "1") +
  agg_point(data = scatter_data, aes = agg_aes(x = x, y = y), panel = "1")
print(p)
p <- arphitgg(long_data, aes = agg_aes(x = date, group = state), layout = "2h") +
  agg_line(aes = agg_aes(y = y), panel = "1") +
  agg_col(aes = agg_aes(y = y2), panel = "3")
print(p)
p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y), color = RBA["Red1"])
print(p)

# SERIES COLORS AND OTHER ATTRIBUTES ===============

graphics.off()
p <- arphitgg() +
  agg_line(data = long_data,
           aes = agg_aes(x = date, y = y, group = state),
           color = RBA["Red1"])
print(p)
p <- arphitgg() +
  agg_line(data = long_data,
           aes = agg_aes(x = date, y = y, group = state),
           color = c(RBA["Red1"], RBA["Blue4"]))
print(p)

# TITLES ============

graphics.off()
p <- arphitgg() +
  agg_title("Graph Title Goes Here") +
  agg_subtitle("Or a subtitle, if you like")
print(p)
p <- arphitgg() + agg_title("Graph Title Goes Here") +
  agg_subtitle("Or a subtitle, if you like") +
  agg_title("And here is panel title", panel = "1") +
  agg_subtitle("Panel subtitle too if needed", panel = "1")
print(p)

# UNITS ==============

graphics.off()
p <- arphitgg() + agg_units("index")
print(p)
p <- arphitgg() +
  agg_units("index", panel = "1") +
  agg_units("ppt", panel = "2")
print(p)

# SOURCES AND FOOTNOTES =============

graphics.off()
p <- arphitgg() + agg_source("Source 1") +
  agg_source(c("Source 2 (as a vector)", "Source 3 (vectors are easy!")) +
  agg_footnote("This is my first footnoote") +
  agg_footnote("This is a second footnote")
print(p)
