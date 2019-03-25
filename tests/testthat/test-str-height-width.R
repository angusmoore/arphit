context("str-height-width")

test_strings <- c(
  "The quick brown fox",
  "Having\ntwo lines",
  "Some unicode \U2013 characters too",
  "Even more lines\nwith\nbreaks and stuff AND CAPITALS and $%!$#*#"
)

test_that("Width", {
  print(arphitgg()) # Start a canvas, so that R's string height is calibrated

  sapply(test_strings, function(x)
    expect_equal(
      strwidth(x, units = "inches"),
      getstrwidth(x, units = "inches"),
      tolerance = 1e-7
    ))
})

test_that("Height", {
  print(arphitgg()) # Start a canvas, so that R's string height is calibrated

  sapply(test_strings, function(x)
    expect_equal(
      strheight(x, units = "inches"),
      getstrheight(x, units = "inches"),
      tolerance = 1e-7
    ))
})

dev.off()
