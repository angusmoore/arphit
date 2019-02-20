

inchesasuser_height <- function(x) {
  x*(graphics::par("usr")[4] - graphics::par("usr")[3])/graphics::par("pin")[2]
}

getstrheight <- function(text, units = "inches", cex = 1) {
  single_height <- 0.21875
  break_height <- 0.1620115
  n_lines <- stringr::str_count(text, stringr::fixed("\n"))
  inch_height <- cex * (single_height * (n_lines + 1) + break_height * n_lines)
  if (units == "user") {
    return(inchesasuser_height(inch_height))
  } else if (units == "inches") {
    return(inch_height)
  }
}

inchesasuser_width <- function(x) {
  x*(graphics::par("usr")[2] - graphics::par("usr")[1])/graphics::par("pin")[1]
}

charwidth <- function(C) {
  widths <- list(
    "a" = 0.15625,
    "b" = 0.15625,
    "c" = 0.145833333333333,
    "d" = 0.15625,
    "e" = 0.15625,
    "f" = 0.0729166666666667,
    "g" = 0.15625,
    "h" = 0.15625,
    "i" = 0.0625,
    "j" = 0.0625,
    "k" = 0.145833333333333,
    "l" = 0.0625,
    "m" = 0.229166666666667,
    "n" = 0.15625,
    "o" = 0.15625,
    "p" = 0.15625,
    "q" = 0.15625,
    "r" = 0.09375,
    "s" = 0.145833333333333,
    "t" = 0.0833333333333333,
    "u" = 0.15625,
    "v" = 0.135416666666667,
    "w" = 0.197916666666667,
    "x" = 0.125,
    "y" = 0.145833333333333,
    "z" = 0.135416666666667,
    "A" = 0.1875,
    "B" = 0.1875,
    "C" = 0.208333333333333,
    "D" = 0.208333333333333,
    "E" = 0.1875,
    "F" = 0.177083333333333,
    "G" = 0.21875,
    "H" = 0.197916666666667,
    "I" = 0.0833333333333333,
    "J" = 0.135416666666667,
    "K" = 0.1875,
    "L" = 0.15625,
    "M" = 0.239583333333333,
    "N" = 0.197916666666667,
    "O" = 0.21875,
    "P" = 0.177083333333333,
    "Q" = 0.21875,
    "R" = 0.208333333333333,
    "S" = 0.1875,
    "T" = 0.166666666666667,
    "U" = 0.197916666666667,
    "V" = 0.177083333333333,
    "W" = 0.291666666666667,
    "X" = 0.177083333333333,
    "Y" = 0.1875,
    "Z" = 0.177083333333333,
    "0" = 0.15625,
    "1" = 0.15625,
    "2" = 0.15625,
    "3" = 0.15625,
    "4" = 0.15625,
    "5" = 0.15625,
    "6" = 0.15625,
    "7" = 0.15625,
    "8" = 0.15625,
    "9" = 0.15625,
    " "  = 0.08333333,
    "-" = 0.09375,
    "\U2013" = 0.15625,
    "\U2014" = 0.28125,
    "!" = 0.083333333,
    "@" = 0.28125,
    "#" = 0.15625,
    "$" = 0.15625,
    "%" = 0.25,
    "^" = 0.07291667,
    "&" = 0.1875,
    "*" = 0.1145833,
    "(" = 0.09375,
    ")" = 0.09375,
    "+" = 0.1666667,
    "=" = 0.1666667,
    "`" = 0.09375,
    "~" = 0.1666667,
    "'" = 0.05208333,
    '"' = 0.1041667,
    "/" = 0.08333333,
    "\\" = 0.08333333,
    "]" = 0.08333333,
    "[" = 0.08333333,
    "{" = 0.05208333,
    "}" = 0.05208333,
    "|" = 0.03125,
    ":" = 0.08333333,
    ";" = 0.08333333,
    ">" = 0.1666667,
    "<" = 0.1666667,
    "?" = 0.15625,
    "." = 0.04166667,
    "," = 0.04166667,
    "\U20AC" = 0.2291667,
    "\U00A3" = 0.2083333,
    "\U00A5" = 0.2083333
  )
  width <- widths[[C]]
  if (is.null(width)) {
    warning(paste0("No width for ", C))
    return(0.291666666666667)
  } else {
    return(width)
  }
}

getstrlinewidth <- function(text) {
  if (nchar(text) == 0) return(0)
  text <- strsplit(text, "")[[1]]
  sum(sapply(text, charwidth, USE.NAMES=FALSE))
}

getstrwidth <- function(text, units = "inches", cex = 1) {
  if (is.null(text) || text == "") return(0)
  lines <- stringr::str_split(text,stringr::fixed("\n"))[[1]]
  widths <- sapply(lines, getstrlinewidth, USE.NAMES = FALSE)
  inch_width <- max(widths)*cex
  if (units == "user") {
    return(inchesasuser_width(inch_width))
  } else if (units == "inches") {
    return(inch_width)
  }
}
