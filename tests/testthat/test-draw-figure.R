context("Draw figure")

height <- 10 + (10 + 20)*CSI
width <- 20 + (30 + 40)*CSI
expect_equal(getfigsize(c(10,20), 10, 20, 30, 40)$height, height)
expect_equal(getfigsize(c(10,20), 10, 20, 30, 40)$width, width)

height <- 10 + (1 + 2)*CSI
width <- 20 + 2*MINIMUMSIDEPADDING
expect_equal(getfigsize(c(10,20), 1, 2, 0.3, 0.4)$height, height)
expect_equal(getfigsize(c(10,20), 1, 2, 0.3, 0.4)$width, width)

height <- 10 + (1 + 2)*CSI
width <- 20 + MINIMUMSIDEPADDING + 6*CSI
expect_equal(getfigsize(c(10,20), 1, 2, 1, 6)$height, height)
expect_equal(getfigsize(c(10,20), 1, 2, 1, 6)$width, width)

expect_error(handlelayout("sdf"))

expect_that(countfnlines("asdf"), equals(3.5))
expect_that(countfnlines(list("asdf","asdf")), equals(4.6))
expect_that(countfnlines(list("asdf","as\ndf")), equals(5.7))
expect_that(countfnlines(list("as\ndf","as\ndf")), equals(6.8))

expect_that(counttitlelines("foo", NULL), equals(2.7))
expect_that(counttitlelines(NULL, NULL), equals(0.99))
expect_that(counttitlelines(NULL, "foo"), equals(2.14))
expect_that(counttitlelines("Foo", "foo"), equals(3.85))
expect_that(counttitlelines("Foo\nbar", "foo"), equals(5.75))
expect_that(counttitlelines("Foo\nbar", "foo\nbar"), equals(7.55))

expect_that(countsrclines(list(text = "ASDF", plural = FALSE)), equals(1.7))
expect_that(countsrclines(list(text = "ASDF, Foo, Bar", plural = TRUE)), equals(1.7))
expect_that(countsrclines(list(text = "ASDF, FoA\no, Bar", plural = TRUE)), equals(2.8))

expect_that(is.even(2), is_true())
expect_that(is.even(3), is_false())

expect_error(finddevice("abc.xyz"))
expect_that(finddevice("abc.png"), equals("png"))
expect_that(finddevice("abc.pdf"), equals("pdf"))
expect_that(finddevice("abc.emf"), equals("emf"))
expect_that(finddevice(NULL), equals(NULL))

# test that each of the filenames output the right type of file
randomdata <- ts(data.frame(x1 = rnorm(12)), start = c(2000,1), frequency = 4)
for (suffix in c("png","pdf","emf")) {
  file <- paste("foo.", suffix, sep = "")
  arphit(randomdata, filename = file)
  # test exists
  expect_that(file.exists(file), is_true())
  # remove
  file.remove(file)
}

# tests for bottom spacing
fakeseries1 <- c("a","b")
onesided <- handlepanels(fakeseries1, "1")
expect_equal(figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list(), 0, LANDSCAPESIZE, FALSE)$bottomskip, 0)
expect_equal(figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list("1" = "test"), 0, LANDSCAPESIZE, FALSE)$bottomskip, 1.7)

# tests for extra margins when have y axis labels
noaxislabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list(), 0,  LANDSCAPESIZE, FALSE)$left
yaxislabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list("1" = "foo"), list(), 0, LANDSCAPESIZE, FALSE)$left
expect_that(yaxislabelmargin, is_more_than(noaxislabelmargin))

# tests for extra margins when have x axis labels
noaxislabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list(), 0, LANDSCAPESIZE, FALSE)$bottom
xaxislabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list("1" = "foo"), 0, LANDSCAPESIZE, FALSE)$bottom
expect_that(xaxislabelmargin, is_more_than(noaxislabelmargin))

# tests for extra margins with legends
nolegendlabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list(), 0,  LANDSCAPESIZE, FALSE)$bottom
legendlabelmargin <- figuresetup("", NULL, onesided, list(), list("1" = "%"), NULL, NULL, NULL, list(text = "", plural = FALSE), list(), list(), 1,  LANDSCAPESIZE, FALSE)$left
expect_that(legendlabelmargin, is_more_than(nolegendlabelmargin))
