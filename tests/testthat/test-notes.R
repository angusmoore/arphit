context("Panel titles")
fakeseries1 <- c("a", "b")
fakeseries2 <- c("c", "d")
panels1 <- handlepanels(list("1" = fakeseries1, "2" = fakeseries2), NULL, "1")
panels2h <- handlepanels(list("1" = fakeseries1, "2" = fakeseries2), NULL, "2h")

expect_that(conformpaneltitles(panels2h, list("1" = "Foo", "3" = "Bar")), equals(list("1" = "Foo", "2" = NULL, "3" = "Bar", "4" = NULL)))
expect_that(conformpaneltitles(panels1, list("1" = "Foo")), equals(list("1" = "Foo", "2" = NULL)))
expect_that(conformpaneltitles(panels1, "Baz"), equals(list("1" = "Baz", "2" = "Baz")))

context("Format sources")
expect_that(formatsrcs("ABS"), equals(list(text = "ABS", plural = FALSE)))
expect_that(formatsrcs(c("ABS")), equals(list(text = "ABS", plural = FALSE)))
expect_that(formatsrcs(c("ABS","RBA")), equals(list(text = "ABS; RBA", plural = TRUE)))

context("Format footnotes")
foo <- c("A footnote", "B footnote")
expect_that(formatfn(foo), equals(foo))
# This also checks split over lines
foo <- c("A footnote", "A veeeeeeeeeeeeery loooooooooooong foooooootnoooote that should split over lines")
expect_that(formatfn(foo)[2], equals("A veeeeeeeeeeeeery loooooooooooong foooooootnoooote\nthat should split over lines"))
