context("Integration - qplot options")

data <- ts(data.frame(x1 = rnorm(12),
                      x2 = rnorm(12),
                      x3 = rnorm(12, sd = 10),
                      x4 = rnorm(12, sd = 5)),
           start = c(2000,1),
           frequency = 4)

# DATA AND TYPES OF GRAPHS =================

graphics.off()

expect_error(
  agg_qplot(data),
  NA)

mytsdata <- ts(data.frame(x1 = rnorm(10)), frequency = 4, start = c(2000, 1))
expect_error(
  agg_qplot(mytsdata),
  NA)

mydataframe <- data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                          length.out = 10, by = "quarter"),
                          x1 = rnorm(10))
expect_error(
  agg_qplot(mydataframe, x = "date"),
  NA)

mydataframe <- data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                          length.out = 10, by = "quarter"),
                          x1 = rnorm(10))
expect_error(
  agg_qplot(mydataframe, x = "date"),
  NA)

categoricaldata <- data.frame(categoryname = letters[1:5], value = rnorm(5))
expect_error(
  agg_qplot(categoricaldata, x = "categoryname"),
  NA)

scatterdata <- data.frame(x = rnorm(20), y = rnorm(20))
expect_error(
  agg_qplot(scatterdata, x = "x"),
  NA)

data1 <- data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                    length.out = 10, by = "quarter"),
                    x1 = rnorm(10))
data2 <- data.frame(years = seq.Date(from = as.Date("2000-01-01"),
                                     length.out = 4, by = "year"),
                    x2 = rnorm(4))
expect_error(
  agg_qplot(data = list("1" = data1, "2" = data2),
            x = list("1" = "date", "2" = "years"),
            layout = "2v"),
  NA)

# PANELS, SERIES, LAYOUTS =================

graphics.off()
expect_error(
  agg_qplot(data, layout = "1", series = list("1" = c("x1","x4"))),
  NA)
expect_error(
  agg_qplot(data, layout = "1", series = list("1" = c("x1"), "2" = c("x4"))),
  NA)
expect_error(
  agg_qplot(data, layout = "2v", series = list("1" = c("x1"), "2" = c("x2"))),
  NA)
expect_error(
  agg_qplot(data, layout = "2h", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4"))),
  NA)
expect_error(
  agg_qplot(data, layout = "2h", series = list("1" = c("x1","x2"), "3" = c("x3","x4"))),
  NA)
expect_error(
  agg_qplot(data, layout = "2b2", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4"))),
  NA)
expect_error(
  agg_qplot(data, layout = "3v", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"))),
  NA)
expect_error(
  agg_qplot(data, layout = "3h", series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x1"), "5" = "x1")),
  NA)
expect_error(
  agg_qplot(data, layout = "3b2",
            series = list("1" = "x1", "2" = "x2", "3" = "x1", "4" = "x2", "5" = "x1", "6" = "x4")),
  NA)
expect_error(
  agg_qplot(data, layout = "4h",
          series = list("1" = "x1", "2" = "x2", "3" = "x1",
                        "4" = "x2", "5" = "x1", "6" = "x4",
                        "7" = "x2", "8" = c("x1","x2"))),
  NA)
expect_error(
  agg_qplot(data, layout = "4b2",
          series = list("1" = "x1", "2" = "x2", "3" = "x1",
                        "4" = "x2", "5" = "x1", "6" = "x4",
                        "7" = "x2", "8" = c("x1","x2"))),
  NA)
expect_error(
  agg_qplot(data),
  NA)
expect_error(
  agg_qplot(data, portrait = TRUE),
  NA)
expect_error(
  agg_qplot(data, plotsize = c(2,10)),
  NA)

# BAR GRAPHS =================

graphics.off()
expect_error(
  agg_qplot(data, series = list("1" = c("x1","x2"), "2" = c("x3")), bars = c("x2")),
  NA)
expect_error(
  agg_qplot(data, series = list("1" = c("x1","x2")), bars = c("x1","x2")),
  NA)
expect_error(
  agg_qplot(data, bars = c("x1","x2"), bar.stacked = FALSE),
  NA)

# TITLES AND PANELS ====================

graphics.off()
expect_error(
  agg_qplot(data, title = "Here is a Title", subtitle = "And a subtitle"),
  NA)
expect_error(
  agg_qplot(data,
            title = "Here is a very very very long title that arphit will automatically put a break in",
            subtitle = "And a subtitle\nwith a break too"),
  NA)
expect_error(
  agg_qplot(data, layout = "2b2",
            series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")),
            paneltitle = list("1" = "Panel 1", "2" = "Panel 2", "3" = "Panel 3", "4" = "Panel 4")),
  NA)

# FOOTNOTES AND SOURCES =================

graphics.off()
expect_error(
  agg_qplot(data, footnote = c("Footnote 1", "Footnote 2", "etc, this can go on for a while")),
  NA)
expect_error(
  agg_qplot(data, sources = c("RBA", "ABS", "someone else")),
  NA)

# Y AXIS UNITS ===========

graphics.off()
expect_error(
  agg_qplot(data,
            layout = "2b2",
            series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")),
            yunits = list("1" = "index", "2" = "ppt", "3" = "$", "4" = "000s")),
  NA)
expect_error(
  agg_qplot(data,
            layout = "2b2",
            series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")),
            yunits = "000s"),
  NA)
scatterdata <- data.frame(x = rnorm(20), y = rnorm(20))
expect_error(
  agg_qplot(scatterdata, x = "x", xunits = "ppt"),
  NA)

# LINE, BAR AND MARKER OPTIONS ===============

graphics.off()
expect_error(
  agg_qplot(data, col = list("x2" = "pink", "x4" = "lightblue")),
  NA)
expect_error(
  agg_qplot(data, pch = 19),
  NA)
expect_error(
  agg_qplot(data,
            series = list("1" = c("x2", "x4")),
            col = list("x2" = RBA["Red1"], "x4" = RBA["Blue10"])),
  NA)

# AXIS LIMITS ==============

graphics.off()
expect_error(
  agg_qplot(data,
            series = list("1" = c("x1"), "2" = c("x3")),
            ylim = list("1" = list(min = -10, max = 10, nsteps = 5),
                        "2" = list(min = -20, max = 20, nsteps = 5))),
  NA)
expect_error(
  agg_qplot(data,
            layout = "2b2",
            series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")),
            ylim = list(min = -10, max = 10, nsteps = 5)),
  NA)
expect_error(
  agg_qplot(data,
            layout = "2b2",
            series = list("1" = c("x1"), "2" = c("x2"), "3" = c("x3"), "4" = c("x4")),
            dropxlabel = TRUE),
  NA)

# SHADING BETWEEN SERIES ============

graphics.off()
expect_error(
  agg_qplot(data, shading = list(list(from = "x1", to = "x2", color= "red"))),
  NA)
expect_error(
  agg_qplot(data,
            shading = list(list(from = "x1", to = "x2", color= "red"),
                           list(from = "x3", to = "x4", color = "purple"))),
  NA)

# PLOT ANNOTATIONS ==============

graphics.off()
expect_error(
  agg_qplot(data,
            labels = list(list(x = 2001, y = 2, text = "A label",
                               panel = 1, color = "red"),
                          list(x = 2004, y = 10, text = "A label\n(On two lines!)",
                               panel = 1, color = "green"))),
  NA)
expect_error(
  agg_qplot(data,
            arrows = list(list(tail.x = 2000, tail.y = 10,
                               head.x = 2001, head.y = 1,
                               color = "darkred", panel = 1))),
  NA)
expect_error(
  agg_qplot(data,
            lines = list(list(x = 2001, panel = 1),
                         list(y = -1, color = "darkred", panel = 1, lty = 2))),
  NA)
expect_error(
  agg_qplot(data, enable_autolabeller = TRUE),
  NA)
expect_error(
  agg_qplot(data,
            series = list("1" = c("x1","x2"), "2" = c("x1","x2")),
            layout = "2v",
            labels = list(list(x = 2001, y = 2, text = "A label", panel = "2", color = "red")),
            enable_autolabeller = TRUE),
  NA)
expect_error(
  agg_qplot(data, lines = list(list(x1 = 2000, y1 = -10, x2 = 2002, y2 = 5, panel = 1))),
  NA)
expect_error(
  agg_qplot(data, bgshading = list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 3, panel = 1))),
  NA)
expect_error(
  agg_qplot(data,
            layout = "2h",
            series = list("1" = "x4", "3" = "x2"),
            bgshading = list(list(x1 = NA, y1 = -1,
                                  x2 = NA, y2 = 3,
                                  panel = 1),
                             list(x1 = 2000.5, y1 = NA,
                                  x2 = 2001.5, y2 = NA,
                                  panel = 3, color = "lightgreen"))),
  NA)
expect_error(
  agg_qplot(data, layout = "2b2",
            yaxislabels = "A y axis label",
            xaxislabels = "An x label"),
  NA)
expect_error(
  agg_qplot(data, layout = "2b2",
            yaxislabels = list("1" = "Foo", "3" = "Bar"),
            xaxislabels = list("3" = "An x label", "4" = "Another x label")),
  NA)
expect_error(
  agg_qplot(categoricaldata, x = "categoryname", srt = 45),
  NA)

# JOINING AND MISSING VALUES ==============

graphics.off()
joining_data <- ts(data.frame(y = c(1,2,NA,3,4)), start = 2000, frequency = 1)
expect_error(
  agg_qplot(joining_data, joined = TRUE),
  NA)
expect_error(
  agg_qplot(joining_data, joined = FALSE),
  NA)

# ADDING A LEGEND =============

graphics.off()
expect_error(
  agg_qplot(data, legend = TRUE),
  NA)
