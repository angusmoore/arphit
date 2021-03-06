test_that("Smoke tests", {
  # Hard to test output, so just run smoke tests

  # Some mutlipanel graphs with metadata
  facet_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5), y = 1:10,
    group = c("a", "a", "b", "b", "a", "a", "a", "b", "b", "b"),
    facet = c("c", "c", "c", "c", "c", "d", "d", "d", "d", "d"),
    stringsAsFactors = FALSE)
  bar <- arphitgg(facet_data,
                  agg_aes(x = x, y = y, facet = facet, group = group)) +
    agg_line() +
    agg_title("Title") +
    agg_subtitle("Subtitle") +
    agg_footnote(c("A footnote", "A second footnote")) +
    agg_source(c("A source", "Another source"))
  agg_draw(bar, "test.xlsx")
  expect_true(file.exists("test.xlsx"))

  # Time series data
  data <- data.frame(x1 = rnorm(30),
                     agg_time = seq.Date(from  = as.Date("2001-03-01"),
                                         by = "quarter",
                                         length.out = 30))

  p <- arphitgg(data, agg_aes(x = agg_time, y = x1)) + agg_line()
  agg_draw(p, "test-2.xlsx")
  expect_true(file.exists("test-2.xlsx"))

  # With xlimit (#307)
  p <- arphitgg(data, agg_aes(x = agg_time, y = x1)) + agg_line() +
    agg_xlim(2006, NA)
  agg_draw(p, "test-3.xlsx")
  expect_true(file.exists("test-3.xlsx"))

  p <- arphitgg(data, agg_aes(x = agg_time, y = x1)) + agg_line() +
    agg_xlim(NA, 2006)
  agg_draw(p, "test-4.xlsx")
  expect_true(file.exists("test-4.xlsx"))

  # Correct number of rows #348
  p <- arphitgg(data.frame(x = letters[1:10],
                           y = rnorm(10),
                           stringsAsFactors = FALSE),
                agg_aes(x, y)) + agg_col()
  agg_draw(p, "baz.xlsx")
  baz <- readxl::read_xlsx("baz.xlsx", sheet = "1")
  expect_equal(nrow(baz), 10)
})

test_that("Characters coerced to factors (#280)", {
  foo <- data.frame(date = "AAA",
                    x = letters[1:10],
                    y = rnorm(10),
                    z = rnorm(10),
                    stringsAsFactors = FALSE)
  p <- arphitgg(foo, agg_aes(x = x, y = y, group = date)) +
    agg_col() + agg_col(agg_aes(y = z)) +
    agg_autolabel()
  expect_warning(agg_draw(p, "foo.xlsx"), NA)
  file.remove("foo.xlsx")
})
