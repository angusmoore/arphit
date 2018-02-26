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

# Set colours
foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), color = "red")
bar <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), color = c("red", "green"))
baz <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) + agg_line(aes = agg_aes(x = date, y = employment, group = state), color = c("red", "green"))
foo2 <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state)) + agg_line(aes = agg_aes(x = date, y = employment, group = state), color = c("red", "green"), panel = "2")
expect_that(foo$col, equals(list("1" = list("A" = "red", "B" = "red", "C" = "red", "D" = "red"))))
expect_that(bar$col, equals(list("1" = list("A" = "red", "B" = "green", "C" = "red", "D" = "green"))))
expect_that(foo2$col, equals(list("2" = list("A" = "red", "B" = "green", "C" = "red", "D" = "green"))))

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
expect_that(foo$scaleunits, equals("index"))
foo <- arphitgg(data) + agg_units("index", panel = "1") + agg_units("bar", panel = "4")
expect_that(foo$scaleunits[["1"]], equals("index"))
expect_that(foo$scaleunits[["4"]], equals("bar"))

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
