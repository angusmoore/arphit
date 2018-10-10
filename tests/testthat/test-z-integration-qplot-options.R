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

# ADDING A LEGEND =============

graphics.off()
expect_error(
  agg_qplot(data, legend = TRUE),
  NA)

# ERROR IF PASS VECTOR TO SERIES ==============

expect_error(
  agg_qplot(data, series = "x1"),
  "`series` must be a list mapping panel names to vector of series to be included in that panel."
)
