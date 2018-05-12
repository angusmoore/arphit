simple_data <- data.frame(date = seq.Date(from = as.Date("2000-03-01"),
                                          length.out = 10,
                                          by = "quarter"),
                          y1 = rnorm(10),
                          y2 = rnorm(10),
                          y3 = rnorm(10),
                          y4 = rnorm(10))
long_data <- data.frame(date = rep(seq.Date(from = as.Date("2000-03-01"),
                                            length.out = 10,
                                            by = "quarter"), 2),
                        y1 = rnorm(20),
                        y2 = rnorm(20),
                        group_var = c(rep("A", 10), rep("B", 10)))
scatter_data <- data.frame(x = rnorm(10), y = rnorm(10))

# LAYOUTS AND PANELS =================

graphics.off()
p <- arphitgg(layout = "1")
print(p)
p <- arphitgg(layout = "2v")
print(p)
p <- arphitgg(layout = "2h")
print(p)
p <- arphitgg(layout = "2b2")
print(p)
p <- arphitgg(layout = "3v")
print(p)
p <- arphitgg(layout = "3h")
print(p)
p <- arphitgg(layout = "3b2")
print(p)
p <- arphitgg(layout = "4h")
print(p)
p <- arphitgg(layout = "4b2")
print(p)

p <- arphitgg(portrait = TRUE)
print(p)
p <- arphitgg(plotsize = c(2,10))
print(p)

# LAYERS AND AESTHETICS ===============

graphics.off()
p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y1))
print(p)
p <- arphitgg(data = simple_data, aes = agg_aes(x = date, y = y1)) +
  agg_line()
print(p)
p <- arphitgg(data = simple_data, aes = agg_aes(x = date)) +
  agg_line(aes = agg_aes(y = y1))
print(p)
p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var))
print(p)
p <- arphitgg() +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, facet = group_var))
print(p)
facet_data <- data.frame(x = rep(c(1,2),4),
                         y = rnorm(8),
                         age = c(rep("A", 4), rep("B", 4)),
                         sex = rep(c("F","F","M","M"), 2))
p <- arphitgg() +
  agg_line(data = facet_data, aes = agg_aes(x = x, y = y, group = sex, facet = age))
print(p)
p <- arphitgg(layout = "2h") +
  agg_line(data = long_data, aes = agg_aes(x = date, y = y1, group = group_var), panel = "1") +
  agg_col(data = long_data, aes = agg_aes(x = date, y = y2, group = group_var), panel = "3")
print(p)
p <- arphitgg(layout = "1") +
  agg_point(data = data.frame(x = rnorm(10), y = rnorm(10)), aes = agg_aes(x = x, y = y), panel = "1")
print(p)
p <- arphitgg(long_data, aes = agg_aes(x = date, group = group_var), layout = "2h") +
  agg_line(aes = agg_aes(y = y1), panel = "1") +
  agg_col(aes = agg_aes(y = y2), panel = "3")
print(p)
p <- arphitgg() +
  agg_line(data = simple_data, aes = agg_aes(x = date, y = y1), color = RBA["Red1"])
print(p)
p <- arphitgg() +
  agg_line(data = long_data,
           aes = agg_aes(x = date, y = y1, group = group_var),
           color = RBA["Red1"])
print(p)
p <- arphitgg() +
  agg_line(data = long_data,
           aes = agg_aes(x = date, y = y1, group = group_var),
           color = c(RBA["Red1"], RBA["Blue4"]))
print(p)
p <- arphitgg(simple_data) +
  agg_line(agg_aes(x = date, y = y1), pch = 19)
print(p)
p <- arphitgg(simple_data) +
  agg_line(agg_aes(x = date, y = y1), lty = 2)
print(p)
p <- arphitgg(simple_data) +
  agg_line(agg_aes(x = date, y = y1), lwd = 3)
print(p)
p <- arphitgg(simple_data) +
  agg_col(agg_aes(x = date, y = y1), barcol = "black")
print(p)

# TITLES AND SUBTITLES =================

graphics.off()
p <- arphitgg() +
  agg_title("Graph Title Goes Here") +
  agg_subtitle("Or a subtitle, if you like")
print(p)
p <- arphitgg() +
  agg_title("Here is a very very very long title that arphit will automatically put a break in") +
  agg_subtitle("And a subtitle\nwith a manual break too")
print(p)

# PANEL TITLES AND SUBTITLES =================

graphics.off()
p <- arphitgg() +
  agg_title("Panel title for panel 1", panel = "1") +
  agg_subtitle("And a subtitle", panel = "1")
print(p)

# UNITS =================

p <- arphitgg() + agg_units("index")
print(p)
p <- arphitgg() +
  agg_units("index", panel = "1") +
  agg_units("ppt", panel = "2")
print(p)
p <- arphitgg(scatter_data, agg_aes(x=x,y=y)) +
  agg_point() + agg_xunits("ppt")
print(p)

# SOURCES AND FOOTNOTES ==================

graphics.off()
p <- arphitgg() +
  agg_source("Source 1") +
  agg_source(c("Source 2 (as a vector)", "Source 3 (vectors are easy!")) +
  agg_footnote("This is my first footnoote") +
  agg_footnote("This is a second footnote")
print(p)

# AXIS LIMITS ==================

graphics.off()
p <- arphitgg(simple_data) +
  agg_line(agg_aes(x=date,y=y1),panel="1") +
  agg_line(agg_aes(x=date,y=y1),panel="2") +
  agg_ylim(-1,1,5,panel="1") +
  agg_ylim(-4,4,5,panel="2")
print(p)
p <- arphitgg(simple_data) +
  agg_line(agg_aes(x=date,y=y1),panel="1") +
  agg_line(agg_aes(x=date,y=y1),panel="2") +
  agg_ylim(-1,1,5)
print(p)
p <- arphitgg(simple_data, agg_aes(x=date, y=y1)) +
  agg_line() +
  agg_xlim(1998, 2008)
print(p)

# SHADING BETWEEN SERIES ============

graphics.off()
p <- arphitgg(simple_data, agg_aes(x=date)) +
  agg_line(agg_aes(y=y1)) +
  agg_line(agg_aes(y=y2)) +
  agg_shading(from = y1, to = y2, color = RBA["Blue1"])
print(p)
p <- arphitgg(simple_data, agg_aes(x=date)) +
  agg_line(agg_aes(y=y1)) +
  agg_line(agg_aes(y=y2)) +
  agg_line(agg_aes(y=y3)) +
  agg_line(agg_aes(y=y4)) +
  agg_shading(from = y1, to = y2, color = RBA["Blue1"]) +
  agg_shading(from = y3, to = y4, color = "lightgrey")
print(p)

# PLOT ANNOTATIONS ===================

graphics.off()
p <- arphitgg() +
  agg_label("A label", x = 2002, y = 0.5, color = RBA["Blue2"], panel = "1")
print(p)
p <- arphitgg() +
  agg_arrow(tail.x = 2000, tail.y = 0, head.x = 2001, head.y = 0.5, color = RBA["Blue1"], panel = "1")
print(p)
p <- arphitgg() +
  agg_abline(x = 2001, panel = "1") +
  agg_abline(y = -1, color = "darkred", lty = 2, panel = "1")
print(p)
p <- arphitgg() +
  agg_abline(x1 = 2000, y1 = -10, x2 = 2002, y2 = 5, panel = "1")
print(p)
p <- arphitgg() +
  agg_bgshading(x1 = NA, y1 = -0.5, x2 = NA, y2 = 0.5, panel = "1")
print(p)
p <- arphitgg(layout = "2h") +
  agg_bgshading(x1 = NA, y1 = -0.5, x2 = NA, y2 = 0.5, panel = "1") +
  agg_bgshading(x1 = 2000.5, y1 = NA, x2 = 2001.5, y2 = NA, panel = "3", color = "lightgreen")
print(p)
p <- arphitgg(layout = "2b2") +
  agg_yaxislabel("A y axis label") +
  agg_xaxislabel("An x label")
print(p)
p <- arphitgg(layout = "2b2") +
  agg_yaxislabel("Foo", panel = "1") +
  agg_yaxislabel("Bar", panel = "3") +
  agg_xaxislabel("An x label", panel = "3") +
  agg_xaxislabel("Another x label", panel = "4")
print(p)
p <- arphitgg(simple_data, agg_aes(x=date,y=y1), srt = 45) + agg_line()
print(p)

# JOINING AND MISSING DATA ==============

graphics.off()
joining_data <- ts(data.frame(y = c(1,2,NA,3,4)), start = 2000, frequency = 1)
p <- arphitgg(joining_data, agg_aes(y = y), joined = TRUE) +
  agg_line()
print(p)
p <- arphitgg(joining_data, agg_aes(y = y), joined = FALSE) +
  agg_line()
print(p)

# LEGEND ==============

graphics.off()
p <- arphitgg(long_data, agg_aes(x = date, y = y1, group = group_var)) +
  agg_line() +
  agg_legend()
print(p)
