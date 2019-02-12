context("Notes ")

## Title ===================

test_that("Title", {
  p <- arphitgg() + agg_title("Test")
  expect_true(check_graph(p, "notes-title"))

  p <- arphitgg(layout = "2b2") + agg_title("Test", panel = "1") + agg_title("Test 3", panel = "3")
  expect_true(check_graph(p, "notes-paneltitles"))

  p <- arphitgg() +
    agg_title("This is a veeeeeerrrrrryy loooooooooonnnnnnngg title that should break across lines") +
    agg_title("This is a veeeeeerrrrrryy loooooooooonnnnnnngg panel title too", panel = "1")
  expect_true(check_graph(p, "notes-long-title"))

  p <- arphitgg() +
    agg_title("This is a veeeeeerrrrrryy\nloooooooooonnnnnnngg title\nwith manual line breaks") +
    agg_title("And manual\nline\nbreaks in a veeeeeeeeeeeeeeeeeeeeeerrrrrrryyyyy loong title", panel = "1")
  expect_true(check_graph(p, "notes-long-title-manual-breaks"))
})


## Subtitle ===================

test_that("Subtitle", {
  p <- arphitgg() + agg_subtitle("Test")
  expect_true(check_graph(p, "notes-subtitle"))

  p <- arphitgg(layout = "2b2") + agg_subtitle("Test", panel = "1") + agg_subtitle("Test 3", panel = "3")
  expect_true(check_graph(p, "notes-panelsubtitles"))

  p <- arphitgg() +
    agg_subtitle("This is a veeeeeerrrrrryy loooooooooonnnnnnngg title that should break across lines") +
    agg_subtitle("This is a veeeeeerrrrrryy loooooooooonnnnnnngg panel title too", panel = "1")
  expect_true(check_graph(p, "notes-long-subtitle"))

  p <- arphitgg() +
    agg_subtitle("This is a veeeeeerrrrrryy\nloooooooooonnnnnnngg title\nwith manual line breaks") +
    agg_subtitle("And manual\nline\nbreaks in a veeeeeeeeeeeeeeeeeeeeeerrrrrrryyyyy loong title", panel = "1")
  expect_true(check_graph(p, "notes-long-subtitle-manual-breaks"))
})

test_that("Subtitle and title", {
  p <- arphitgg() +
    agg_title("Here's a title, and some other stuff") +
    agg_subtitle("Now a subtitle, which is fun") +
    agg_title("Here's a title, and some other stuff", panel = "1") +
    agg_subtitle("Now a subtitle, which is fun", panel = "1")
  expect_true(check_graph(p, "notes-title-and-subtitle"))
})

## Footnotes ===================

test_that("Footnotes", {
  p <- arphitgg() + agg_footnote("This is a footnote") + agg_footnote("second footnote")
  expect_true(check_graph(p, "notes-footnotes"))
  p <- arphitgg() + agg_footnote(c("This is a footnote", "second footnote"))
  expect_true(check_graph(p, "notes-footnotes2"))
  p <- arphitgg() + agg_footnote("Just one footnote this time")
  expect_true(check_graph(p, "notes-footnote"))

  skip("Linebreaks not working right (#233)")
  p <- arphitgg() +
    agg_footnote(c("A footnote", "A veeeeeeeeeeeeery loooooooooooong foooooootnoooote that should split over lines"))
  expect_true(check_graph(p, "notes-footnote-long"))

  p <- arphitgg() +
    agg_footnote(c("A footnote", "An already split\nveeeeeeeeeeeeery loooooooooooong foooooootnoooote that should split over lines"))
  expect_true(check_graph(p, "notes-footnote-long-manual-breaks"))
})

## Source ===================

test_that("Sources", {
  p <- arphitgg() + agg_source("One source")
  expect_true(check_graph(p, "notes-source"))
  p <- arphitgg() + agg_source(c("One source", "Two source"))
  expect_true(check_graph(p, "notes-sources"))
  p <- arphitgg() + agg_source("One source") + agg_source("Three source")
  expect_true(check_graph(p, "notes-sources2"))
})
