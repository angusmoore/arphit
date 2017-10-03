# Needed set up
fakeseries1 <- c("a","b")
panels <- handlepanels(fakeseries1, NULL, "1")

context("Series attribute handling")
# All defaults
defaultattributes <- handleattributes(panels$series, NULL, NULL, NULL, NULL, NULL)
expect_that(defaultattributes$col, equals(list("a" = DEFAULTCOLORS[1], "b" = DEFAULTCOLORS[2])))
expect_that(defaultattributes$pch, equals(list("a" = DEFAULTPCH, "b" = DEFAULTPCH)))
expect_that(defaultattributes$lty, equals(list("a" = DEFAULTLTY, "b" = DEFAULTLTY)))
expect_that(defaultattributes$lwd, equals(list("a" = DEFAULTLWD, "b" = DEFAULTLWD)))
expect_that(defaultattributes$barcol, equals(list("a" = DEFAULTBARCOL, "b" = DEFAULTBARCOL)))

# Now passing in just one series, the other should be defaults
oneattribute <- handleattributes(panels$series, list("a" = "green"), list("b" = 6), list("a" = 5), list("b" = 10), list("a" = "purple"))
expect_that(oneattribute$col, equals(list("a" = "green", "b" = DEFAULTCOLORS[1])))
expect_that(oneattribute$pch, equals(list("a" = DEFAULTPCH, "b" = 6)))
expect_that(oneattribute$lty, equals(list("a" = 5, "b" = DEFAULTLTY)))
expect_that(oneattribute$lwd, equals(list("a" = DEFAULTLWD, "b" = 10)))
expect_that(oneattribute$barcol, equals(list("a" = "purple", "b" = DEFAULTBARCOL)))

# Passing attributes for all series
allfromsingle <- handleattributes(panels$series, "pink", 4, 5, 22, "pink")
expect_that(allfromsingle$col, equals(list("a" = "pink", "b" = "pink")))
expect_that(allfromsingle$pch, equals(list("a" = 4, "b" = 4)))
expect_that(allfromsingle$lty, equals(list("a" = 5, "b" = 5)))
expect_that(allfromsingle$lwd, equals(list("a" = 22, "b" = 22)))
expect_that(allfromsingle$barcol, equals(list("a" = "pink", "b" = "pink")))
