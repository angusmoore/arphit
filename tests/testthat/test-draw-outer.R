context("draw outer")

# Legend

# Set up panels
onesided <- handlepanels(c("a","b"), "1")
twosided <- handlepanels(list("1" = c("a","b"), "2" = c("c","d")), "1")
duplicate <- handlepanels(list("1" = c("a","b"), "2" = c("a","d")), "1")

# Set attributes
os_attr <- handleattributes(onesided,
                            list(a = "red", b = "green"),
                            list(a = 1, b = 2),
                            list(a = 3, b = 4),
                            list(a = 5, b = 6),
                            list(a = 7, b = 8),
                            list(a = 1, b = 1))
ts_attr <- handleattributes(twosided,
                            list(a = "red", b = "green", c = "blue", d = "black"),
                            list(a = 1, b = 2, c = 1, d = 2),
                            list(a = 3, b = 4, c = 3, d = 4),
                            list(a = 5, b = 6, c = 5, d = 6),
                            list(a = 7, b = 8, c = 7, d = 8),
                            list(a = 1, b = 1, c = 1, d = 1))
dup_attr <- handleattributes(duplicate,
                             list(a = "red", b = "green", d = "black"),
                             list(a = 1, b = 2, d = 2),
                             list(a = 3, b = 4, d = 4),
                             list(a = 5, b = 6, d = 6),
                             list(a = 7, b = 8, d = 8),
                             list(a = 1, b = 1, d = 1))
dup_diff_attr <- handleattributes(duplicate,
                                 list("1" = list(a = "red", b = "green"),
                                      "2" = list(a = "blue", d = "black")),
                                 list(),
                                 list(),
                                 list(),
                                 list(),
                                 list())

# get legend entries
expect_equal(length(getlegendentries(onesided, list(), os_attr)), 2)
expect_equal(length(getlegendentries(twosided, list(), ts_attr)), 4)
expect_equal(length(getlegendentries(duplicate, list(), dup_attr)), 3)
expect_equal(length(getlegendentries(duplicate, list(), dup_diff_attr)), 4)
expect_equal(length(getlegendentries(duplicate, list("1" = "a", "2" = "a"), dup_attr)), 3)
expect_equal(length(getlegendentries(duplicate, list("1" = "a"), dup_attr)), 4)

# extracting the attributes for the legends
expect_equal(sapply(getlegendentries(onesided, list(), os_attr), FUN = extract_item, item = "pch"), c(1, 2))
expect_equal(sapply(getlegendentries(twosided, list(), ts_attr), FUN = extract_item, item = "col"), c("red", "green", "blue", "black"))
expect_equal(sapply(getlegendentries(duplicate, list(), dup_attr), FUN = extract_item, item = "pch"), c(1, 2, 2))
expect_equal(sapply(getlegendentries(duplicate, list(), dup_diff_attr), FUN = extract_item, item = "col"), c("red", "green", "blue", "black"))

expect_equal(sapply(getlegendentries(duplicate, list("1" = "a", "2" = "a"), dup_attr), FUN = extract_item, item = "pch"), c(NA, 2, 2))
expect_equal(sapply(getlegendentries(duplicate, list("1" = "a"), dup_attr), FUN = extract_item, item = "pch"), c(NA, 2, 1, 2))

# determining legend size
expect_equal(determinelegendcols(list("1" = c("foo", "Bar", "baz")), NA), list(r = 1, c = 3))
longname <- paste0(rep("A",15), collapse="")
expect_equal(determinelegendcols(list("1" = c(paste0(longname,1), paste0(longname,2), paste0(longname,3))), NA), list(r = 2, c = 2))

# smoke tests
data <- ts(data.frame(x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10)), frequency = 4, start = c(2000,1))
expect_error(agg_qplot(data, legend=TRUE), NA)
expect_error(agg_qplot(data, legend = TRUE, bars = "x1"), NA)
expect_error(agg_qplot(data, legend = TRUE, bars = "x1", pch = list("x2" = ".")), NA)
colnames(data) <- c(paste0(longname,1), paste0(longname,2), paste0(longname,3))
expect_error(agg_qplot(data, legend = TRUE), NA)
