context("Duplicate series handling")
series <- list("1" = c("x1","x2"), "2" = c("x1","x3"), "3" = c("x2","x4"))
renamedseries <- list("1" = c("x1.1","x2.1"), "2" = c("x1.2","x3"), "3" = c("x2.3","x4"))
dups <- c("x1","x2")
dupmap <- list("x1" = c("1","2"), "x2" = c("1","3"))
T <- 24
randomdata <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10), x4 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 4)

expect_that(finddups(series), equals(dups))
expect_that(findduppanels(dups, series), equals(dupmap))
expect_that(renameseries(dupmap, series), equals(renamedseries))
expect_that(colnames(duplicatedata(dupmap, randomdata)), equals(c(colnames(randomdata),"x1.1","x1.2","x2.1","x2.3")))

bars <- c("x1", "x4")
expect_that(duplicatebars(dupmap, bars), equals(c("x4","x1.1","x1.2")))

col <- list("x2" = "pink", "x4" = "lightblue")
shouldbe <- list("x4" = "lightblue", "x2.1" = "pink", "x2.3" = "pink")
expect_that(duplicateattribute(dupmap, col), equals(shouldbe))
expect_that(duplicateattribute(dupmap, 1), equals(1))
