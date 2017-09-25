# Needed set up
fakeseries1 <- c("a","b")
duplicateseries1 <- c("a","c")
panels <- handlepanels(fakeseries1, NULL, "1")
duplicatepanels <- handlepanels(list("1" = fakeseries1, "2" = duplicateseries1), NULL, "1")

context("Series attribute handling - non-duplicate series")
# All defaults
defaultattributes <- handleattributes(panels$series, panels$duplicates, NULL, NULL, NULL, NULL, NULL)
expect_that(defaultattributes$col, equals(list("a" = DEFAULTCOLORS[1], "b" = DEFAULTCOLORS[2])))
expect_that(defaultattributes$pch, equals(list("a" = DEFAULTPCH, "b" = DEFAULTPCH)))
expect_that(defaultattributes$lty, equals(list("a" = DEFAULTLTY, "b" = DEFAULTLTY)))
expect_that(defaultattributes$lwd, equals(list("a" = DEFAULTLWD, "b" = DEFAULTLWD)))
expect_that(defaultattributes$barcol, equals(list("a" = DEFAULTBARCOL, "b" = DEFAULTBARCOL)))

# Now passing in just one series, the other should be defaults
oneattribute <- handleattributes(panels$series, panels$duplicates, list("a" = "green"), list("b" = 6), list("a" = 5), list("b" = 10), list("a" = "purple"))
expect_that(oneattribute$col, equals(list("a" = "green", "b" = DEFAULTCOLORS[1])))
expect_that(oneattribute$pch, equals(list("a" = DEFAULTPCH, "b" = 6)))
expect_that(oneattribute$lty, equals(list("a" = 5, "b" = DEFAULTLTY)))
expect_that(oneattribute$lwd, equals(list("a" = DEFAULTLWD, "b" = 10)))
expect_that(oneattribute$barcol, equals(list("a" = "purple", "b" = DEFAULTBARCOL)))

# Passing attributes for all series
allfromsingle <- handleattributes(panels$series, panels$duplicates, "pink", 4, 5, 22, "pink")
expect_that(allfromsingle$col, equals(list("a" = "pink", "b" = "pink")))
expect_that(allfromsingle$pch, equals(list("a" = 4, "b" = 4)))
expect_that(allfromsingle$lty, equals(list("a" = 5, "b" = 5)))
expect_that(allfromsingle$lwd, equals(list("a" = 22, "b" = 22)))
expect_that(allfromsingle$barcol, equals(list("a" = "pink", "b" = "pink")))

context("Series attribute handling - duplicate series")
# All defaults
defaultattributes <- handleattributes(duplicatepanels$series, duplicatepanels$duplicates, NULL, NULL, NULL, NULL, NULL)
expect_that(defaultattributes$col, equals(list("a1" = DEFAULTCOLORS[1], "a2" = DEFAULTCOLORS[2], "b" = DEFAULTCOLORS[3], "c" = DEFAULTCOLORS[4])))
expect_that(defaultattributes$pch, equals(list("a1" = DEFAULTPCH, "a2" = DEFAULTPCH, "b" = DEFAULTPCH, "c" = DEFAULTPCH)))
expect_that(defaultattributes$lty, equals(list("a1" = DEFAULTLTY, "a2" = DEFAULTLTY, "b" = DEFAULTLTY, "c" = DEFAULTLTY)))
expect_that(defaultattributes$lwd, equals(list("a1" = DEFAULTLWD, "a2" = DEFAULTLWD, "b" = DEFAULTLWD, "c" = DEFAULTLWD)))
expect_that(defaultattributes$barcol, equals(list("a1" = DEFAULTBARCOL, "a2" = DEFAULTBARCOL, "b" = DEFAULTBARCOL, "c" = DEFAULTBARCOL)))

# Passing attributes for all series
allfromsingle <- handleattributes(duplicatepanels$series, duplicatepanels$duplicates, "pink", 4, 5, 22, "blue")
expect_that(allfromsingle$col, equals(list("a1" = "pink", "a2" = "pink", "b" = "pink", "c" = "pink")))
expect_that(allfromsingle$pch, equals(list("a1" = 4, "a2" = 4, "b" = 4, "c" = 4)))
expect_that(allfromsingle$lty, equals(list("a1" = 5, "a2" = 5, "b" = 5, "c" = 5)))
expect_that(allfromsingle$lwd, equals(list("a1" = 22, "a2" = 22, "b" = 22, "c" = 22)))
expect_that(allfromsingle$barcol, equals(list("a1" = "blue", "a2" = "blue", "b" = "blue", "c" = "blue")))

# Check that adding attributes to one of the non-duplicate series doesn't ruin anything
singlenonduplicate <- handleattributes(duplicatepanels$series, duplicatepanels$duplicates, list("b" = "purple"), list("b" = 6), list("b" = 5), list("b" = 10), list("b" = "grey"))
expect_that(singlenonduplicate$col, equals(list("a1" = DEFAULTCOLORS[1], "a2" = DEFAULTCOLORS[2], "b" = "purple", "c" = DEFAULTCOLORS[3])))
expect_that(singlenonduplicate$pch, equals(list("a1" = DEFAULTPCH, "a2" = DEFAULTPCH, "b" = 6, "c" = DEFAULTPCH)))
expect_that(singlenonduplicate$lty, equals(list("a1" = DEFAULTLTY, "a2" = DEFAULTLTY, "b" = 5, "c" = DEFAULTLTY)))
expect_that(singlenonduplicate$lwd, equals(list("a1" = DEFAULTLWD, "a2" = DEFAULTLWD, "b" = 10, "c" = DEFAULTLWD)))
expect_that(singlenonduplicate$barcol, equals(list("a1" = DEFAULTBARCOL, "a2" = DEFAULTBARCOL, "b" = "grey", "c" = DEFAULTBARCOL)))

# adding attributes to the duplicate series, as named
toduplicate <- handleattributes(duplicatepanels$series, duplicatepanels$duplicates, list("a" = "purple"), list("a" = 6), list("a" = 5), list("a" = 10), list("a" = "orange"))
expect_that(toduplicate$col, equals(list("a1" = "purple", "a2" = "purple", "b" = DEFAULTCOLORS[1], "c" = DEFAULTCOLORS[2])))
expect_that(toduplicate$pch, equals(list("a1" = 6, "a2" = 6, "b" = DEFAULTPCH, "c" = DEFAULTPCH)))
expect_that(toduplicate$lty, equals(list("a1" = 5, "a2" = 5, "b" = DEFAULTLTY, "c" = DEFAULTLTY)))
expect_that(toduplicate$lwd, equals(list("a1" = 10, "a2" = 10, "b" = DEFAULTLWD, "c" = DEFAULTLWD)))
expect_that(toduplicate$barcol, equals(list("a1" = "orange", "a2" = "orange", "b" = DEFAULTBARCOL, "c" = DEFAULTBARCOL)))

# Adding attributes to the duplicate series, individually
individualduplicates <- handleattributes(duplicatepanels$series, duplicatepanels$duplicates, list("a1" = "purple", "a2" = "cyan"), list("a1" = 6, "a2" = 4), list("a1" = 5, "a2" = 7), list("a1" = 10, "a2" = 12), list("a1" = "white", "a2" = "magenta"))
expect_that(individualduplicates$col, equals(list("a1" = "purple", "a2" = "cyan", "b" = DEFAULTCOLORS[1], "c" = DEFAULTCOLORS[2])))
expect_that(individualduplicates$pch, equals(list("a1" = 6, "a2" = 4, "b" = DEFAULTPCH, "c" = DEFAULTPCH)))
expect_that(individualduplicates$lty, equals(list("a1" = 5, "a2" = 7, "b" = DEFAULTLTY, "c" = DEFAULTLTY)))
expect_that(individualduplicates$lwd, equals(list("a1" = 10, "a2" = 12, "b" = DEFAULTLWD, "c" = DEFAULTLWD)))
expect_that(individualduplicates$barcol, equals(list("a1" = "white", "a2" = "magenta", "b" = DEFAULTBARCOL, "c" = DEFAULTBARCOL)))

