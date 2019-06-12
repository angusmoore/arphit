context("str-height-width")

to_test <- c(
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
  "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F",
  "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
  "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", " ", "-",
  "\U2013", "\U2014", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "+", "=",
  "`", "~", "'", '"', "/", "\\", "]", "[", "{", "}", "|", ":", ";", ">", "<", "?",
  ".", ",", "\U20AC", "\U00A3", "\U00A5", "_"
)

print(arphitgg()) # instantiate a canvas to get the correct widths

char_match_width <- function(text) {
  if (!expect_true(abs(strwidth(text, "inches") - getstrwidth(text)) < 1e-5)) {
    cat("Width for: ", text, " is incorrect, (", getstrwidth(text), " versus R-calculated ", strwidth(text, "inches"),")\n")
  }
}

test_that("String width", {
  if (.Platform$OS.type != "windows" || !interactive()) skip("String width tests only work on windows at the moment")
  sapply(to_test, char_match_width)
})

char_match_height <- function(text) {
  if (!expect_equal(abs(strheight(text, "inches") - getstrheight(text)) < 1e-5)) {
    cat("Height for: ", text, " is incorrect\n")
  }
}

test_that("String height", {
  if (.Platform$OS.type != "windows" || !interactive()) skip("String height tests only work on windows at the moment")
  sapply(to_test, char_match_width)
})
