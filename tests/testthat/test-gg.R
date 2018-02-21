context("gg interface")
data  <- data.frame(unemployment = rnorm(20), employment = rnorm(20), employmentYE = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))

# Unrename merged data
foo <- data.frame(a = 1:10, b = 20:29)
bar <- data.frame(a = 1:10, b = 30:39)
expect_that("b" %in% colnames(unrename(dplyr::full_join(foo, bar, by = "a"))), is_true())

# Add line

# Line - Grouped


# Add bar

# Bar - Grouped

# Set colours

# Title

# Subtitle

# Units

# Footnotes

# Source
