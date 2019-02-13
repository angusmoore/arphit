context("Write xlsx")

test_that("Smoke tests", {
  # Hard to test output, so just run smoke tests

  # Some mutlipanel graphs with metadata
  facet_data <- data.frame(x=c(1,2,3,4,5,1,2,3,4,5),y=1:10,group=c("a","a","b","b","a","a","a","b","b","b"),facet=c("c","c","c","c","c","d","d","d","d","d"), stringsAsFactors = FALSE)
  bar <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet,group=group)) +
    agg_line() +
    agg_title("Title") +
    agg_subtitle("Subtitle") +
    agg_footnote(c("A footnote", "A second footnote")) +
    agg_source(c("A source", "Another source"))
  agg_draw(bar, "test.xlsx")
  expect_true(file.exists("test-1.xlsx"))

  # Time series data
  data <-
    data.frame(
      x1 = rnorm(12),
      agg_time = seq.Date(
        from  = as.Date("2001-03-01"),
        by = "quarter",
        length.out = 12
      )
    )

  p <- arphitgg(data, agg_aes(x=agg_time,y=x1)) + agg_line()
  agg_draw(p, "test-2.xlsx")
  expect_true(file.exists("test-1.xlsx"))
})
