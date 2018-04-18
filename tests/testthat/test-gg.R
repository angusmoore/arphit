context("gg interface")
data  <- data.frame(unemployment = rnorm(20), employment = rnorm(20), employmentYE = rnorm(20), state = c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))

# Unrename merged data
foo <- data.frame(a = 1:10, b = 20:29)
bar <- data.frame(a = 1:10, b = 30:39)
expect_that("b" %in% colnames(unrename(dplyr::full_join(foo, bar, by = "a"))), is_true())

# Add line
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment))
expect_that(nrow(foo$data[["1"]]), equals(20))
expect_that(ncol(foo$data[["1"]]), equals(2))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "unemployment")))
expect_that(foo$x[["1"]], equals("date"))

# Line - Grouped
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state))
expect_that(nrow(foo$data[["1"]]), equals(10))
expect_that(ncol(foo$data[["1"]]), equals(5))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "A", "B", "C", "D")))
expect_that(foo$x[["1"]], equals("date"))

# Add bar
foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment))
expect_that(nrow(foo$data[["1"]]), equals(20))
expect_that(ncol(foo$data[["1"]]), equals(2))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "unemployment")))
expect_that(foo$x[["1"]], equals("date"))
expect_that(foo$bars, equals(list("1" = "unemployment")))

# Bar - Grouped
foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state))
expect_that(nrow(foo$data[["1"]]), equals(10))
expect_that(ncol(foo$data[["1"]]), equals(5))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "A", "B", "C", "D")))
expect_that(foo$x[["1"]], equals("date"))
expect_that(foo$bars, equals(list("1" = c("A", "B", "C", "D"))))

# Bar - Grouped duplicates
foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state)) + agg_line(aes = agg_aes(x = date, y = employment, group = state))
expect_that(nrow(foo$data[["1"]]), equals(10))
expect_that(ncol(foo$data[["1"]]), equals(9))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "A", "B", "C", "D", "A.y", "B.y", "C.y", "D.y")))
expect_that(foo$x[["1"]], equals("date"))
expect_that(foo$bars, equals(list("1" = c("A", "B", "C", "D"))))

# Point
foo <- arphitgg(data) + agg_point(aes = agg_aes(x = date, y = unemployment, group = state))
expect_that(foo$pch[["1"]], equals(list("A" = 16, "B" = 16, "C" = 16, "D" = 16)))
expect_that(foo$lty[["1"]], equals(list("A" = 0, "B" = 0, "C" = 0, "D" = 0)))

# Inherited aesthetic
foo <- arphitgg(data, agg_aes(x = date, y = unemployment, group = state)) + agg_line()
bar <- arphitgg(data, agg_aes(x = date)) + agg_line(aes = agg_aes(y = unemployment, group = state))

expect_that(nrow(foo$data[["1"]]), equals(10))
expect_that(ncol(foo$data[["1"]]), equals(5))
expect_that(colnames(foo$data[["1"]]), equals(c("date", "A", "B", "C", "D")))
expect_that(foo$x[["1"]], equals("date"))

expect_that(nrow(bar$data[["1"]]), equals(10))
expect_that(ncol(bar$data[["1"]]), equals(5))
expect_that(colnames(bar$data[["1"]]), equals(c("date", "A", "B", "C", "D")))
expect_that(bar$x[["1"]], equals("date"))

# error without data
expect_error(arphitgg()+agg_line())

# Set colours
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), color = "red")
bar <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), color = c("red", "green"))
baz <- arphitgg(data) +
  agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), color = c("red", "green"))
foo2 <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), color = c("red", "green"), panel = "2")

expect_that(foo$col, equals(list("1" = list("A" = "red", "B" = "red", "C" = "red", "D" = "red"))))
expect_that(bar$col, equals(list("1" = list("A" = "red", "B" = "green", "C" = "red", "D" = "green"))))
expect_that(baz$col, equals(list("1" = list("A.y" = "red", "B.y" = "green", "C.y"= "red", "D.y" = "green"))))
expect_that(foo2$col, equals(list("2" = list("A" = "red", "B" = "green", "C" = "red", "D" = "green"))))

# Set pch
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), pch = 10)
bar <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), pch = c(20, 30))
baz <- arphitgg(data) +
  agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), pch = c(40, 50))
foo2 <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), pch = c(10, 20), panel = "2")

expect_that(foo$pch, equals(list("1" = list("A" = 10, "B" = 10, "C" = 10, "D" = 10))))
expect_that(bar$pch, equals(list("1" = list("A" = 20, "B" = 30, "C" = 20, "D" = 30))))
expect_that(baz$pch, equals(list("1" = list("A.y" = 40, "B.y" = 50, "C.y"= 40, "D.y" = 50))))
expect_that(foo2$pch, equals(list("2" = list("A" = 10, "B" = 20, "C" = 10, "D" = 20))))


# Set lty
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), lty = 10)
bar <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), lty = c(20, 30))
baz <- arphitgg(data) +
  agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), lty = c(40, 50))
foo2 <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), lty = c(10, 20), panel = "2")

expect_that(foo$lty, equals(list("1" = list("A" = 10, "B" = 10, "C" = 10, "D" = 10))))
expect_that(bar$lty, equals(list("1" = list("A" = 20, "B" = 30, "C" = 20, "D" = 30))))
expect_that(baz$lty, equals(list("1" = list("A.y" = 40, "B.y" = 50, "C.y"= 40, "D.y" = 50))))
expect_that(foo2$lty, equals(list("2" = list("A" = 10, "B" = 20, "C" = 10, "D" = 20))))

# Set lwd
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), lwd = 10)
bar <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), lwd = c(20, 30))
baz <- arphitgg(data) +
  agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), lwd = c(40, 50))
foo2 <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) +
  agg_line(aes = agg_aes(x = date, y = employment, group = state), lwd = c(10, 20), panel = "2")

expect_that(foo$lwd, equals(list("1" = list("A" = 10, "B" = 10, "C" = 10, "D" = 10))))
expect_that(bar$lwd, equals(list("1" = list("A" = 20, "B" = 30, "C" = 20, "D" = 30))))
expect_that(baz$lwd, equals(list("1" = list("A.y" = 40, "B.y" = 50, "C.y"= 40, "D.y" = 50))))
expect_that(foo2$lwd, equals(list("2" = list("A" = 10, "B" = 20, "C" = 10, "D" = 20))))

# Set barcol
foo <- arphitgg(data) + agg_col(agg_aes(x = date, y = unemployment, group = state), barcol = "red")
bar <- arphitgg(data) + agg_col(agg_aes(x = date, y = unemployment, group = state), barcol = c("blue", "green"))
baz <- arphitgg(data) +
  agg_col(agg_aes(x = date, y = unemployment, group = state)) +
  agg_col(agg_aes(x = date, y = employment, group = state), barcol = c("red", "green"))
foo2 <- arphitgg(data) + agg_col(agg_aes(x = date, y = unemployment, group = state)) +
  agg_col(agg_aes(x = date, y = employment, group = state), barcol = c("pink", "blue"), panel = "2")

expect_that(foo$barcol, equals(list("1" = list("A" = "red", "B" = "red", "C" = "red", "D" = "red"))))
expect_that(bar$barcol, equals(list("1" = list("A" = "blue", "B" = "green", "C" = "blue", "D" = "green"))))
expect_that(baz$barcol, equals(list("1" = list("A.y" = "red", "B.y" = "green", "C.y"= "red", "D.y" = "green"))))
expect_that(foo2$barcol, equals(list("2" = list("A" = "pink", "B" = "blue", "C" = "pink", "D" = "blue"))))

# Colours with duplicates
foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state), color = "green") + agg_line(aes = agg_aes(x = date, y = employment, group = state), color = "red")
expect_that(foo$col[["1"]], equals(list("A" = "green", "B" = "green", "C" = "green", "D" = "green", "A.y" = "red", "B.y" = "red", "C.y" = "red", "D.y" = "red")))

# Title
foo <- arphitgg(data) + agg_title("Test")
expect_that(foo$title, equals("Test"))
foo <- arphitgg(data) + agg_title("Test", panel = "1") + agg_title("Test 3", panel = "3")
expect_that(foo$paneltitles[["1"]], equals("Test"))
expect_that(foo$paneltitles[["3"]], equals("Test 3"))

# Subtitle
foo <- arphitgg(data) + agg_subtitle("Test")
expect_that(foo$subtitle, equals("Test"))
foo <- arphitgg(data) + agg_subtitle("Test", panel = "1") + agg_subtitle("Test 3", panel = "3")
expect_that(foo$panelsubtitles[["1"]], equals("Test"))
expect_that(foo$panelsubtitles[["3"]], equals("Test 3"))

# Units
foo <- arphitgg(data) + agg_units("index")
expect_that(foo$yunits, equals("index"))
foo <- arphitgg(data) + agg_units("index", panel = "1") + agg_units("bar", panel = "4")
expect_that(foo$yunits[["1"]], equals("index"))
expect_that(foo$yunits[["4"]], equals("bar"))

# Footnotes
foo <- arphitgg(data) + agg_footnote("This is a footnote") + agg_footnote("second footnote")
bar <- arphitgg(data) + agg_footnote(c("This is a footnote", "second footnote"))
expect_that(foo$footnotes, equals(c("This is a footnote", "second footnote")))
expect_that(bar$footnotes, equals(c("This is a footnote", "second footnote")))

foo <- arphitgg(data) + agg_footnote("Just one footnote this time")
expect_that(foo$footnotes, equals("Just one footnote this time"))

# Source
foo <- arphitgg(data) + agg_source("One source")
bar <- arphitgg(data) + agg_source(c("One source", "Two source"))
baz <- arphitgg(data) + agg_source("One source") + agg_source("Three source")
expect_that(foo$sources, equals("One source"))
expect_that(bar$sources, equals(c("One source", "Two source")))
expect_that(baz$sources, equals(c("One source", "Three source")))

# X units
foo <- arphitgg(data) + agg_xunits("foo")
bar <- arphitgg(data) + agg_xunits("bar", panel = "2")
expect_equal(foo$xunits, "foo")
expect_equal(bar$xunits, list("2" = "bar"))

# Labels
foo <- arphitgg(data) + agg_label("Text", "red", 2002, 0.2, "1")
bar <- arphitgg(data) + agg_label("Text", "red", 2002, 0.2, "1") + agg_label("Second label", "green", 2003, -0.2, "1")

expect_equal(foo$labels, list(list(text = "Text", color = "red", x = 2002, y = 0.2, panel = "1")))
expect_equal(bar$labels, list(list(text = "Text", color = "red", x = 2002, y = 0.2, panel = "1"),
                              list(text = "Second label", color = "green", x = 2003, y = -0.2, panel = "1")))

# Arrows
foo <- arphitgg(data) + agg_arrow(2000, 0, 2001, 2, "red", "1")
bar <- arphitgg(data) + agg_arrow(2000, 0, 2001, 2, "red", "1") + agg_arrow(2001, 0, 2002, 2, "green", "1", 2)

expect_equal(foo$arrows, list(list(tail.x = 2000, tail.y = 0, head.x = 2001, head.y = 2, color = "red", panel = "1", lwd = 1)))
expect_equal(bar$arrows, list(list(tail.x = 2000, tail.y = 0, head.x = 2001, head.y = 2, color = "red", panel = "1", lwd = 1),
     list(tail.x = 2001, tail.y = 0, head.x = 2002, head.y = 2, color = "green", panel = "1", lwd = 2)))

# AB lines
foo <- arphitgg(data) + agg_abline(x = 2001, color = RBA["Blue1"], panel = "1")
bar <- arphitgg(data) + agg_abline(y = -0.5, color = RBA["Red1"], panel = "1") +
  agg_abline(x = 2001, color = RBA["Blue1"], panel = "1")
baz <- arphitgg(data) + agg_abline(x1 = 2000, y1 = -0.1, x2 = 2002, y2 = 0.5, panel = "1")

expect_equal(foo$lines, list(list(x = 2001, y = NULL, x1 = NULL, y1 = NULL,
                                  x2 = NULL, y2 = NULL, color = RBA["Blue1"], panel = "1", lwd = 1, lty = 1)))
expect_equal(bar$lines, list(list(x = NULL, y = -0.5, x1 = NULL, y1 = NULL,
                                  x2 = NULL, y2 = NULL, color = RBA["Red1"], panel = "1", lwd = 1, lty = 1),
                             list(x = 2001, y = NULL, x1 = NULL, y1 = NULL,
                                  x2 = NULL, y2 = NULL, color = RBA["Blue1"], panel = "1", lwd = 1, lty = 1)))


expect_equal(baz$lines, list(list(x = NULL, y = NULL, x1 = 2000, y1 = -0.1,
                                  x2 = 2002, y2 = 0.5, color = NULL, panel = "1", lwd = 1, lty = 1)))

# background shading
foo <- arphitgg(data) + agg_bgshading(x1 = 2001, x2 = 2002, panel = "1")
bar <- arphitgg(data) + agg_bgshading(y1 = 0.5, y2 = -0.5, panel = "1")

expect_equal(foo$bgshading, list(list(x1 = 2001, y1 = NA, x2 = 2002, y2 = NA, color = NULL, panel = "1")))
expect_equal(bar$bgshading, list(list(x1 = NA, y1 = 0.5, x2 = NA, y2 = -0.5, color = NULL, panel = "1")))

# ylim
foo <- arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5)
bar <- arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5, panel = "1")

expect_equal(foo$ylim, list(min = -10, max = 10, nsteps = 5))
expect_equal(bar$ylim, list("1" = list(min = -10, max = 10, nsteps = 5)))

# xlim
foo <- arphitgg(data) + agg_xlim(min = -10, max = 10)
bar <- arphitgg(data) + agg_xlim(min = -10, max = 10, panel = "1")

expect_equal(foo$xlim, c(-10,10))
expect_equal(bar$xlim, list("1" = c(-10,10)))

# shading between series
data <- data.frame(date = seq.Date(from = as.Date("2000-03-10"), length.out = 12, by = "month"),
                                   x1 = rnorm(12), x2 = rnorm(12))
foo <- arphitgg(data, agg_aes(x = date)) + agg_line(agg_aes(y = x1), color = RBA["Blue2"]) +
  agg_line(agg_aes(y = x2), color = RBA["Red4"]) +
  agg_shading(from = x1, to = x2)

expect_equal(foo$shading, list(list(from = "x1", to = "x2")))

# Axis labels
## x
foo <- arphitgg(data) + agg_xaxislabel("foo")
bar <- arphitgg(data) + agg_xaxislabel("bar", panel = "1")
expect_equal(foo$xaxislabels, "foo")
expect_equal(bar$xaxislabels, list("1" = "bar"))

## y
foo <- arphitgg(data) + agg_yaxislabel("foo")
bar <- arphitgg(data) + agg_yaxislabel("bar", panel = "1")
expect_equal(foo$yaxislabels, "foo")
expect_equal(bar$yaxislabels, list("1" = "bar"))

# legends
foo <- arphitgg(data) + agg_legend()
bar <- arphitgg(data) + agg_legend(ncol = 5)
expect_equal(foo$legend, TRUE)
expect_equal(bar$legend.ncol, 5)

# Facets
## data
facet_data <- data.frame(x=1:10,y=1:10,group=c("a","a","b","b","a","a","a","b","b","b"),facet=c("c","c","c","c","d","d","d","d","d","d"), stringsAsFactors = FALSE)
foo <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet)) + agg_line()
bar <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet,group=group)) + agg_line()
expect_equal(foo$data[["1"]]$x, 1:4)
expect_equal(foo$data[["3"]]$x, 5:10)
expect_equal(foo$data[["1"]]$y, 1:4)
expect_equal(foo$data[["3"]]$y, 5:10)
expect_equal(bar$data[["1"]]$x, 1:4)
expect_equal(bar$data[["3"]]$x, 5:10)
expect_equal(bar$data[["1"]]$a, c(1,2,NA,NA))
expect_equal(bar$data[["1"]]$b, c(NA,NA,3,4))
expect_equal(bar$data[["3"]]$a, c(5,6,7,NA,NA,NA))
expect_equal(bar$data[["3"]]$b, c(NA,NA,NA,8,9,10))

## attributes
foo <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet)) + agg_line(color = c("red","green"))
bar <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet,group=group)) + agg_line(color = c("red","green"))
baz <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet,group=group)) + agg_col(color = c("red","green"))

expect_equal(foo$col[["1"]]$y, "red")
expect_equal(foo$col[["3"]]$y, "red")
expect_equal(bar$col[["1"]]$a, "red")
expect_equal(bar$col[["1"]]$b, "green")
expect_equal(bar$col[["3"]]$a, "red")
expect_equal(bar$col[["3"]]$b, "green")
expect_equal(baz$col[["3"]]$a, "red")
expect_equal(baz$col[["3"]]$b, "green")

## facet aesthetic inheritance
foo <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet)) + agg_line()
bar <- arphitgg(facet_data) + agg_line(agg_aes(x=x,y=y,facet=facet))
baz <- arphitgg(facet_data, agg_aes(x=x,facet=facet)) + agg_line(agg_aes(y=y))
qux <- arphitgg(facet_data, agg_aes(x=x,y=y)) + agg_line(agg_aes(facet=facet))
expect_equal(foo$data, bar$data)
expect_equal(foo$col, bar$col)
expect_equal(foo$data, baz$data)
expect_equal(foo$col, baz$col)
expect_equal(foo$data, qux$data)
expect_equal(foo$col, qux$col)

## auto facet layouts
facetlayout <- arphit:::facetlayout

two_facets <- data.frame(f = letters[1:2])
expect_equal(facetlayout(two_facets, "f", "1")$layout, "2h")
expect_equal(facetlayout(two_facets, "f", "1")$panels, c("1","3"))
expect_equal(facetlayout(two_facets, "f", "2b2")$layout, "2b2") # override
expect_equal(facetlayout(two_facets, "f", "2b2")$panels, c("1","2"))

three_facets <- data.frame(f = letters[1:3])
expect_equal(facetlayout(three_facets, "f", "1")$layout, "3h")
expect_equal(facetlayout(three_facets, "f", "1")$panels, c("1","3","5"))
expect_equal(facetlayout(three_facets, "f", "3v")$layout, "3v")
expect_equal(facetlayout(three_facets, "f", "3v")$panels, c("1","2","3"))

four_facets <- data.frame(f = letters[1:4])
expect_equal(facetlayout(four_facets, "f", "1")$layout, "2b2")
expect_equal(facetlayout(four_facets, "f", "1")$panels, c("1","2","3","4"))

five_facets <- data.frame(f = letters[1:5])
expect_equal(facetlayout(five_facets, "f", "1")$layout, "3b2")
expect_equal(facetlayout(five_facets, "f", "1")$panels, c("1","2","3","4","5"))

six_facets <- data.frame(f = letters[1:6])
expect_equal(facetlayout(six_facets, "f", "1")$layout, "3b2")
expect_equal(facetlayout(six_facets, "f", "1")$panels, c("1","2","3","4","5","6"))

seven_facets <- data.frame(f = letters[1:7])
expect_equal(facetlayout(seven_facets, "f", "1")$layout, "4b2")
expect_equal(facetlayout(seven_facets, "f", "1")$panels, c("1","2","3","4","5","6","7"))

eight_facets <- data.frame(f = letters[1:8])
expect_equal(facetlayout(eight_facets, "f", "1")$layout, "4b2")
expect_equal(facetlayout(eight_facets, "f", "1")$panels, c("1","2","3","4","5","6","7","8"))

nine_facets <- data.frame(f = letters[1:9])
expect_error(facetlayout(nine_facets, "f", "1"))

expect_equal(facetlayout(three_facets, "f", "4h")$layout, "4h")
expect_equal(facetlayout(three_facets, "f", "4h")$panels, c("1","3","5"))
expect_equal(facetlayout(five_facets, "f", "4h")$layout, "4h")
expect_equal(facetlayout(five_facets, "f", "4h")$panels, c("1","2","3","4","5"))

# Facets adding panel titles (#77)
foo <- data.frame(x=1:10,y=1:10,facet=c("a","a","a","a","a","b","b","b","b","b"), stringsAsFactors = FALSE)
bar <- arphitgg(foo, agg_aes(x=x,y=y,facet=facet))+agg_line()
expect_equal(bar$paneltitles, list("1" = "a", "3" = "b"))

# Tests for time series handling
foo <- ts(data.frame(x1=rnorm(10),x2=rnorm(10)), frequency = 4, start=2000)
bar <- arphitgg() + agg_line(agg_aes(y=x1),data=foo) + agg_col(agg_aes(y=x2),data=foo)
expect_equal(colnames(bar$data[["1"]]), c("agg_time", "x1", "x2"))
expect_error(print(bar), NA)

# Failure if data is grouped by a variable not used in the plot (#85)
foo <- dplyr::group_by(data.frame(x=1:10,y=rnorm(10),unused=letters[1:10]), unused)
bar <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y))
expect_error(print(bar), NA)

# Shutdown any devices
graphics.off()
