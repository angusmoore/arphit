# needed set up
context("Shading")
onesided <- list("1" = c("a","b"), "2" = NULL)
twosided <- list("1" =  c("a","b"), "2" = c("c","d"))

shadeerror <- list(list(from = "x1", to = "x2", color= "red"))
shadewrongpanels <- list(list(from = "a", to = "c", color= "red"))
shadewrongpanels_explicit <- list(list(from = "a", to = "c", color= "red", panel = "1"))
shadefine <- list(list(from = "a", to = "b", color= "red"))
shade2 <- list(list(from = "a", to = "b", color= "red"), list(from = "c", to = "d", color= "red"))
shadenocol <- list(list(from = "a", to = "b"))

expect_error(handleshading(shadeerror, onesided), "Cannot shade with series x2 because it does not exist in any panel.")
expect_error(handleshading(shadewrongpanels, twosided), "Series a from your shading options is not a recognised series in panel 2.")

oneshouldbe <- list("1" = list(list(from = "a", to = "b", color = "red")), "2" = list())
expect_that(handleshading(shadefine, onesided), equals(oneshouldbe))
twoshouldbe <- list("1" = list(list(from = "a", to = "b", color = "red")), "2" = list(list(from = "c", to = "d", color = "red")))
expect_that(handleshading(shade2, twosided), equals(twoshouldbe))
defaultcolor <- list("1" = list(list(from = "a", to = "b", color = DEFAULTSHADINGCOLOR)), "2" = list())
expect_that(handleshading(shadenocol, twosided), equals(defaultcolor))

# Duplicate names in different panels - ambiguous shading
dupnames <- handlepanels(list("1" = c("a","b"), "2" = c("a","c")), "1")
expect_error(handleshading(list(list(from = "c", to = "a", color = "red")), dupnames))
expect_error(handleshading(list(list(from = "c", to = "a", color = "red", panel = "2")), dupnames), NA)
