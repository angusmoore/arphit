context("Panel handling")

# Provide panel ids
expect_equal(handlepanels(list("1" = c("x","y","z")), "1"), list("1" = c("x", "y", "z"), "2" = NULL))
expect_equal(handlepanels(list("1" = c("x","y"), "2" = "z"), "1"), list("1" = c("x", "y"), "2" = "z"))
expect_equal(handlepanels(list("1" = "x", "2" = "x"), "1"), list("1" = "x", "2" = "x"))

# Error if provide panel id not in layout
expect_error(handlepanels(list("3" = "a"), "1"), "Your chosen layout (1) does not have a panel 3.", fixed = TRUE)

# Error if invalid layout
expect_error(handlepanels("x", "foo"), "Unknown layout option foo. Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2.", fixed = TRUE)

context("Bar handling")

data <- list("1" = data.frame(x = 1:10,y=1:10), "2" = data.frame(x=1:10,z=1:10))

expect_that(handlebars(data, TRUE), equals(list("1" = c("x","y"), "2" = c("x","z"))))
expect_that(handlebars(data, "z"), equals(list("2" = "z")))
expect_that(handlebars(data, "x"), equals(list("1" = "x", "2" = "x")))
expect_that(handlebars(data, c("x","z")), equals(list("1"="x","2"=c("x","z"))))
expect_that(handlebars(data, list("2" = c("x","z"))), equals(list("2"=c("x","z"))))
