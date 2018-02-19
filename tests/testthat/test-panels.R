context("Panel handling")

# Basic panel handling - not providing panel ids
expect_that(handlepanels(c("x","y","z"), "1"), equals(list("1" = c("x", "y", "z"), "2" = NULL)))
expect_that(handlepanels(c("x","y","z"), "2h"), equals(list("1" = c("x", "y", "z"), "2" = NULL, "3" = NULL, "4" = NULL)))

# Provide panel ids
expect_that(handlepanels(list("1" = c("x","y","z")), "1"), equals(list("1" = c("x", "y", "z"), "2" = NULL)))
expect_that(handlepanels(list("1" = c("x","y"), "2" = "z"), "1"), equals(list("1" = c("x", "y"), "2" = "z")))

# Error if provide panel id not in layout
expect_error(handlepanels(list("3" = "a"), "1"))

# Duplicate
expect_that(handlepanels(list("1" = "x", "2" = "x"), "1"), equals(list("1" = "x", "2" = "x")))
expect_error(handlepanels(list("1" = c("x1", "x2", "x2")), "1"))

# Error if invalid layout
expect_error(handlepanels("x", "foo"))

context("Bar handling")

panels <- handlepanels(list("1" = c("x","y"), "2" = c("x","z")), "1")
expect_that(handlebars(panels, TRUE), equals(list("1" = c("x","y"), "2" = c("x","z"))))
expect_that(handlebars(panels, "z"), equals(list("2" = "z")))
expect_that(handlebars(panels, "x"), equals(list("1" = "x", "2" = "x")))
expect_that(handlebars(panels, c("x","z")), equals(list("1"="x","2"=c("x","z"))))
expect_that(handlebars(panels, list("2" = c("x","z"))), equals(list("2"=c("x","z"))))
