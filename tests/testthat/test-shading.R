# needed set up
context("Shading")
fakeseries1 <- c("a","b")
onesided <- handlepanels(fakeseries1, NULL, "1")
fakeseries2 <- c("c","d")
twosided <- handlepanels(list("1" = fakeseries1, "2" = fakeseries2), NULL, "1")

shadeerror <- list(list(from = "x1", to = "x2", color= "red"))
shadefine <- list(list(from = "a", to = "b", color= "red"))
shade2 <- list(list(from = "a", to = "b", color= "red"), list(from = "c", to = "d", color= "red"))
shadenocol <- list(list(from = "a", to = "b"))

expect_error(handleshading(shadeerror, onesided))

oneshouldbe <- list("1" = list(list(from = "a", to = "b", color = "red")), "2" = list())
expect_that(handleshading(shadefine, onesided), equals(oneshouldbe))
twoshouldbe <- list("1" = list(list(from = "a", to = "b", color = "red")), "2" = list(list(from = "c", to = "d", color = "red")))
expect_that(handleshading(shade2, twosided), equals(twoshouldbe))
defaultcolor <- list("1" = list(list(from = "a", to = "b", color = DEFAULTSHADINGCOLOR)), "2" = list())
expect_that(handleshading(shadenocol, twosided), equals(defaultcolor))
