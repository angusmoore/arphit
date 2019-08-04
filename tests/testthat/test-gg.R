context("gg interface")

## Basic tests of adding layers ==================
library(dplyr)
set.seed(42)
data  <-
  data.frame(
    unemployment = rnorm(20),
    employment = rnorm(20),
    employmentYE = rnorm(20),
    state = c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5)),
    date = seq.Date(
      from = as.Date("2017-01-10"),
      length.out = 10,
      by = "quarter"
    )
  )

test_that("Layers", {
  # Add line
  foo <- arphitgg(filter(data, state == "A")) + agg_line(aes = agg_aes(x = date, y = unemployment))
  expect_true(check_graph(foo, "gg-layer-line"))

  # Line - Grouped
  foo <- arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state))
  expect_true(check_graph(foo, "gg-layer-line-grouped"))

  # Add bar
  foo <- arphitgg(filter(data, state == "A")) + agg_col(aes = agg_aes(x = date, y = unemployment))
  expect_true(check_graph(foo, "gg-layer-bar"))

  # Add two bars
  foo <- arphitgg(filter(data, state == "A"), agg_aes(x = date, y = unemployment)) + agg_col() + agg_col()
  expect_true(check_graph(foo, "gg-layer-2bar"))

  # Bar - Grouped
  foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state))
  expect_true(check_graph(foo, "gg-layer-bar-grouped"))

  # Bar - Grouped duplicate series names
  foo <- arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state)) + agg_line(aes = agg_aes(x = date, y = employment, group = state))
  expect_true(check_graph(foo, "gg-layer-duplicate-series"))

  # Point
  foo <- arphitgg(data) + agg_point(aes = agg_aes(x = date, y = unemployment, group = state))
  expect_true(check_graph(foo, "gg-layer-point"))

  # Simple test of step graph
  foo <- arphitgg(data.frame()) + agg_step(agg_aes(x=1:10,y=1:10))
  expect_true(check_graph(foo, "gg-layer-simple-step"))

  # More complex step
  foo <- arphitgg(data) + agg_step(aes = agg_aes(x = date, y = unemployment, group = state))
  expect_true(check_graph(foo, "gg-layer-step"))

  # Waterfall graph
  data  <- data.frame(x = letters[1:6], y = c(2,1.1,-0.5,-0.2,0.4,2.8))
  foo <- arphitgg(data) + agg_waterfall(agg_aes(x=x,y=y))
  expect_true(check_graph(foo, "gg-layer-waterfall"))

  # Below the axis
  data  <- data.frame(x = letters[1:6], y = -c(2,1,-0.5,-0.2,0.4,2.7))
  foo <- arphitgg(data) + agg_waterfall(agg_aes(x=x,y=y))
  expect_true(check_graph(foo, "gg-layer-waterfall-below"))

  # Cross the axis
  data <- data.frame(x = letters[1:4],
                     y = c(0.5, -1, 0.2, -0.3))
  foo <- arphitgg(data) + agg_waterfall(agg_aes(x,y))
  expect_true(check_graph(foo, "gg-layer-waterfall-crossing"))

  # Only positive changes
  data  <- data.frame(x = letters[1:4], y = c(2,1,0.5,3.5))
  foo <- arphitgg(data) + agg_waterfall(agg_aes(x=x,y=y))
  expect_true(check_graph(foo, "gg-layer-waterfall-positive-changes"))

  # Waterfall with groups
  data <- data.frame(x = c('start','a','a','b','b','end'),
                     y = c(1, 0.5, -0.4, 0.2, 0.1, 1.4),
                     group = c(1, 2, 3, 2, 3, 4),
                     order = c(1,2,2,3,3,4))
  foo <- arphitgg(data) +
    agg_waterfall(agg_aes(x=x,y=y,group=group,order=order))
  expect_true(check_graph(foo, "gg-layer-waterfall-groups"))

  # Waterfall mixed with other layers
  data <- data.frame(x=letters[1:6],y=c(1,0.5,-0.2,0.2,-0.5,1),z=1:6)
  foo <- arphitgg(data) + agg_waterfall(agg_aes(x,y,x))+agg_point(agg_aes(x,z))
  expect_true(check_graph(foo, "gg-layer-waterfall-with-point"))
})

test_that("Aesthetic inheritance", {
  # Inherited aesthetic
  bar <- arphitgg(data, agg_aes(x = date)) + agg_line(aes = agg_aes(y = unemployment, group = state))
  expect_true(check_graph(bar, "gg-layer-line-grouped")) # should exactly match the above test for layer-line-grouped

  baz <- arphitgg(data, agg_aes(x = date, y = unemployment, group = state)) + agg_line()
  expect_true(check_graph(baz, "gg-layer-line-grouped")) # should exactly match the above test for layer-line-grouped

  qux <- arphitgg(data, agg_aes(group = state)) + agg_line(agg_aes(x = date, y = unemployment))
  expect_true(check_graph(qux, "gg-layer-line-grouped")) # should exactly match the above test for layer-line-grouped
})

test_that("Grouping by x variable",{
  data <- data.frame(x=rep(letters[1:10],3),y=1:30)
  p <- arphitgg(data, agg_aes(x=x,y=y,group=x))+agg_line(pch = 19)
  expect_true(check_graph(p, "gg-layer-group-by-x"))
})

## Facets ====================
## data
facet_data <- data.frame(x=c(1,2,3,4,5,1,2,3,4,5),y=1:10,group=c("a","a","b","b","a","a","a","b","b","b"),facet=c("c","c","c","c","c","d","d","d","d","d"), stringsAsFactors = FALSE)

test_that("Facets", {
  foo <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet)) + agg_line()
  bar <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet,group=group)) + agg_line()
  expect_true(check_graph(foo, "gg-facet"))
  expect_true(check_graph(bar, "gg-facet-group"))

  # Facet variable is a factor (#88)
  foo <- data.frame(x=c(1,1,2,2),b=c("a","b","a","b"),y=1:4)
  p <- arphitgg(foo,agg_aes(x=x,y=y,facet=b)) + agg_line()
  expect_true(check_graph(p, "gg-facet-factor"))

  # NAs in facets (#256)
  foo <- data.frame(x=1:10,y=3,g=rep(c(1,2,3,NA,5),2))
  p <- arphitgg(foo, agg_aes(x=x,y=y,facet=g)) + agg_col()
  expect_true(check_graph(p, "gg-facet-na"))
})

## facet aesthetic inheritance
test_that("Facet aesthetic inheritance", {
  foo <- arphitgg(facet_data, agg_aes(x=x,y=y,facet=facet)) + agg_line()
  bar <- arphitgg(facet_data) + agg_line(agg_aes(x=x,y=y,facet=facet))
  baz <- arphitgg(facet_data, agg_aes(x=x,facet=facet)) + agg_line(agg_aes(y=y))
  qux <- arphitgg(facet_data, agg_aes(x=x,y=y)) + agg_line(agg_aes(facet=facet))
  expect_true(check_graph(foo, "gg-facet-aes-inheritance"))
  expect_true(check_graph(bar, "gg-facet-aes-inheritance"))
  expect_true(check_graph(baz, "gg-facet-aes-inheritance"))
  expect_true(check_graph(qux, "gg-facet-aes-inheritance"))
})

## auto facet layouts
test_that("Auto facet layouts", {
  two_facets <- data.frame(f = letters[1:2])
  facet <- rlang::sym("f")
  facet <- rlang::enquo(facet)
  expect_equal(facetlayout(two_facets, facet, "1")$layout, "2h")
  expect_equal(facetlayout(two_facets, facet, "1")$panels, c("1","3"))
  expect_equal(facetlayout(two_facets, facet, "2b2")$layout, "2b2") # override
  expect_equal(facetlayout(two_facets, facet, "2b2")$panels, c("1","2"))

  three_facets <- data.frame(f = letters[1:3])
  expect_equal(facetlayout(three_facets, facet, "1")$layout, "3h")
  expect_equal(facetlayout(three_facets, facet, "1")$panels, c("1","3","5"))
  expect_equal(facetlayout(three_facets, facet, "3v")$layout, "3v")
  expect_equal(facetlayout(three_facets, facet, "3v")$panels, c("1","2","3"))

  four_facets <- data.frame(f = letters[1:4])
  expect_equal(facetlayout(four_facets, facet, "1")$layout, "2b2")
  expect_equal(facetlayout(four_facets, facet, "1")$panels, c("1","2","3","4"))

  five_facets <- data.frame(f = letters[1:5])
  expect_equal(facetlayout(five_facets, facet, "1")$layout, "3b2")
  expect_equal(facetlayout(five_facets, facet, "1")$panels, c("1","2","3","4","5"))

  six_facets <- data.frame(f = letters[1:6])
  expect_equal(facetlayout(six_facets, facet, "1")$layout, "3b2")
  expect_equal(facetlayout(six_facets, facet, "1")$panels, c("1","2","3","4","5","6"))

  seven_facets <- data.frame(f = letters[1:7])
  expect_equal(facetlayout(seven_facets, facet, "1")$layout, "4b2")
  expect_equal(facetlayout(seven_facets, facet, "1")$panels, c("1","2","3","4","5","6","7"))

  eight_facets <- data.frame(f = letters[1:8])
  expect_equal(facetlayout(eight_facets, facet, "1")$layout, "4b2")
  expect_equal(facetlayout(eight_facets, facet, "1")$panels, c("1","2","3","4","5","6","7","8"))

  nine_facets <- data.frame(f = letters[1:9])
  expect_error(facetlayout(nine_facets, facet, "1"))

  expect_equal(facetlayout(three_facets, facet, "4h")$layout, "4h")
  expect_equal(facetlayout(three_facets, facet, "4h")$panels, c("1","3","5"))
  expect_equal(facetlayout(five_facets, facet, "4h")$layout, "4h")
  expect_equal(facetlayout(five_facets, facet, "4h")$panels, c("1","2","3","4","5"))
})

test_that("Facet panel titles", {
  # Facets adding panel titles (#77)
  foo <- data.frame(x=c(1,2,3,4,5,1,2,3,4,5),y=1:10,facet=c("a","a","a","a","a","b","b","b","b","b"), stringsAsFactors = FALSE)
  bar <- arphitgg(foo, agg_aes(x=x,y=y,facet=facet))+agg_line()
  expect_true(check_graph(bar, "gg-facet-panel-titles"))
})

test_that("Facet order", {
  # Order facets
  foo <- data.frame(x=c("a","b","a","b"),y=1:4,facet=c("b","b","a","a"), stringsAsFactors = FALSE)
  bar <- arphitgg(foo,agg_aes(x=x,y=y,facet=facet))+agg_col()
  expect_true(check_graph(bar, "gg-facet-order"))
})

## Error messages ===================
test_that("Error messages", {
  # More helpful error messages if forget to pass in required parts of
  # aesthetic (#130)
  expect_error(
    tibble(y = rnorm(10)) %>% arphitgg(agg_aes(y = y)) + agg_line(),
    "Cannot add layer. You have not specified an x aesthetic (and there was not one to inherit).",
    fixed = TRUE
  )

  expect_error(
    tibble(x = rnorm(10)) %>%  arphitgg(agg_aes(x = x)) + agg_line(),
    "Cannot add layer. You have not specified a y aesthetic for at least one of your layers (and there was not one to inherit).",
    fixed = TRUE
  )

  # error without data
  expect_error(arphitgg()+agg_line(), "You have not supplied data for series")

  # variable not in data
  data <- data.frame(x=1:10,y=1:10)
  expect_error(
    print(arphitgg(data, agg_aes(x=x,y=y1)) + agg_line()),
    "y1 is not in your data for panel 1",
    fixed = TRUE
  )
  expect_error(
    print(arphitgg(data, agg_aes(x=x1,y=y)) + agg_line()),
    "x1 is not in your data for panel 1",
    fixed = TRUE
  )
  expect_error(
    print(arphitgg(data, agg_aes(x=x,y=y,group=group)) + agg_line()),
    "group is not in your data for panel 1",
    fixed = TRUE
  )

  # different classes of x variable
  data <- data.frame(x=1:10,y=1:10)
  data1 <- data.frame(x=letters[1:10],y=1:10, stringsAsFactors = FALSE)
  expect_error(
    print(
      arphitgg(aes = agg_aes(x = x, y = y)) +
        agg_line(data = data) +
        agg_line(data = data1)
    ),
    "Do not know how to join together x values character and integer (panel 1)",
    fixed = TRUE
  )

  data <- data.frame(x=letters[1:10],y=1:10)
  expect_error(
    arphitgg() +
      agg_col(data = filter(data, x != "c"), aes = agg_aes(x = x, y = y, order = y)) +
      agg_col(data = filter(data, x == "c"), aes = agg_aes(x = x, y = y)),
    "Do not know how to join together ordering variables with classes character and integer (panel 1). Perhaps you added layers to the same panel with different ordering variables (or didn't specify an ordering variable for one of the layers)?",
    fixed = TRUE
  )
})

## Ordering ====================

test_that("Ordering", {
  # ordering categorical graphs by value of a variable (#138)
  data <- tibble::tibble(x = letters[1:10], y=1:10, group = "A", order = letters[10:1]) %>%
    bind_rows(tibble::tibble(x = letters[1:10], y=1:10, group = "B", order = letters[10:1]))

  foo <- data %>%
    arphitgg(agg_aes(x=x,y=y,group=group,order=order)) + agg_col()
  expect_true(check_graph(foo, "gg-ordered"))
  bar <- data %>%
    arphitgg(agg_aes(x=x,y=y,group=group)) + agg_col()
  expect_true(check_graph(bar, "gg-unordered"))

  # ordering without groups
  data <- tibble(x = letters[1:10], y=1:10, order = letters[10:1])

  foo <- data %>%
    arphitgg(agg_aes(x=x,y=y,order=order)) + agg_col()
  expect_true(check_graph(foo, "gg-ordered-nogroup"))

  # ordering by y variable (#168)
  set.seed(42)
  foo <- tibble(x=letters[1:15],y=rnorm(15)) %>%
    arphitgg(agg_aes(x=x,y=y,order=y)) + agg_col()
  expect_true(check_graph(foo, "gg-ordered-y"))

  # Order by the value of one of the groups, rather than a separate variable (#162)
  foo <- tibble::tibble(
    x = rep(1:10, 2),
    y = c(1:10, 10:1),
    group = c(rep("A", 10), rep("B", 10))
  ) %>%
    arphitgg(agg_aes(x = x, y = y, group = group, order = A)) + agg_line()
  expect_true(check_graph(foo, "gg-ordered-group-value"))

  foo <- tibble::tibble(
    x = rep(1:10, 2),
    y = c(1:10, 10:1),
    group = c(rep("A", 10), rep("B", 10))
  ) %>%
    arphitgg(agg_aes(x = x, y = y, group = group, order = B)) + agg_line()
  expect_true(check_graph(foo, "gg-ordered-group-value-reversed"))

  # what if the ordering group doesn't have all x values
  foo <- tibble::tibble(
    x = c(1:10, 1:9),
    y = c(1:10, 10:2),
    group = c(rep("A", 10), rep("B", 9))
  )
  expect_error(arphitgg(foo, agg_aes(x=x,y=y,group=group,order=B)) + agg_col(),
               "Ordering is ambiguous - some x values associate with multiple values of the ordering variable, or there are no observations of the ordering variable for some x values.")
})

## Reference multiple panels in one constructor (#191) ======================
test_that("Mutiple panel constructors", {
  # Line layer to multiple panels
  data1 <- tibble(x=rnorm(15),y=rnorm(15),z=rnorm(15))
  p1 <- data1 %>%
    arphitgg(layout = "2b2")+
    agg_line(agg_aes(x=x,y=y), panel = c("1","3"))

  p2 <- data1 %>%
    arphitgg(layout = "2b2")+
    agg_line(agg_aes(x=x,y=y), panel = "1") +
    agg_line(agg_aes(x=x,y=y), panel = "3")

  expect_equal(p1, p2)

  # Bar layer to multiple panels
  p1 <- data1 %>%
    arphitgg(layout = "2b2")+
    agg_col(agg_aes(x=x,y=y), panel = c("1","3"))

  p2 <- data1 %>%
    arphitgg(layout = "2b2")+
    agg_col(agg_aes(x=x,y=y), panel = "1") +
    agg_col(agg_aes(x=x,y=y), panel = "3")

  expect_equal(p1, p2)

  # Title
  p1 <- arphitgg() + agg_title("Foo", panel = c("1", "2"))
  p2 <- arphitgg() + agg_title("Foo", panel = "1") + agg_title("Foo", panel = "2")
  expect_equal(p1, p2)

  # Subtitle
  p1 <- arphitgg() + agg_subtitle("Foo", panel = c("1", "2"))
  p2 <- arphitgg() + agg_subtitle("Foo", panel = "1") + agg_subtitle("Foo", panel = "2")
  expect_equal(p1, p2)

  # units
  p1 <- arphitgg() + agg_units("Foo", panel = c("1", "2"))
  p2 <- arphitgg() + agg_units("Foo", panel = "1") + agg_units("Foo", panel = "2")
  expect_equal(p1, p2)

  # xunits
  p1 <- arphitgg() + agg_xunits("Foo", panel = c("1", "2"))
  p2 <- arphitgg() + agg_xunits("Foo", panel = "1") + agg_xunits("Foo", panel = "2")
  expect_equal(p1, p2)

  # label
  p1 <- arphitgg() + agg_label("Foo", x = 2000, y = 1, colour = "black", panel = c("1", "2"))
  p2 <- arphitgg() +
    agg_label("Foo", x = 2000, y = 1, colour = "black", panel = "1") +
    agg_label("Foo", x = 2000, y = 1, colour = "black", panel = "2")
  expect_equal(p1, p2)

  # arrow
  p1 <- arphitgg() + agg_arrow(0,0,1,1,"black", panel = c("1","2"))
  p2 <- arphitgg() + agg_arrow(0,0,1,1,"black", panel = "1") +
    agg_arrow(0,0,1,1,"black", panel = "2")
  expect_equal(p1, p2)

  # abline
  p1 <- arphitgg() + agg_vline(x=1,colour="black",panel=c("1","2"))
  p2 <- arphitgg() + agg_vline(x=1,colour="black",panel="1") +
    agg_vline(x=1,colour="black",panel="2")
  expect_equal(p1, p2)

  # bgshading
  p1 <- arphitgg() + agg_bgshading(0,0,1,1,"black", panel = c("1","2"))
  p2 <- arphitgg() + agg_bgshading(0,0,1,1,"black", panel = "1") +
    agg_bgshading(0,0,1,1,"black", panel = "2")
  expect_equal(p1, p2)

  # shading
  p1 <- arphitgg() + agg_shading("x2","x1",panel=c("1","2"))
  p2 <- arphitgg() + agg_shading("x2","x1",panel="1") + agg_shading("x2","x1",panel="2")
  expect_equal(p1, p2)

  # ylim
  p1 <- arphitgg() + agg_ylim(0,1,2,panel = c("1","2"))
  p2 <- arphitgg() + agg_ylim(0,1,2,panel = "1") + agg_ylim(0,1,2,panel = "2")
  expect_equal(p1, p2)

  # xlim
  p1 <- arphitgg() + agg_xlim(0,1,panel = c("1","2"))
  p2 <- arphitgg() + agg_xlim(0,1,panel = "1") + agg_xlim(0,1,panel = "2")
  expect_equal(p1, p2)

  # yaxislabel
  p1 <- arphitgg() + agg_yaxislabel("FOO", panel = c("1","2"))
  p2 <- arphitgg() + agg_yaxislabel("FOO", panel = "1") + agg_yaxislabel("FOO", panel = "2")
  expect_equal(p1, p2)

  # xaxislabel
  p1 <- arphitgg() + agg_xaxislabel("FOO", panel = c("1","2"))
  p2 <- arphitgg() + agg_xaxislabel("FOO", panel = "1") + agg_xaxislabel("FOO", panel = "2")
  expect_equal(p1, p2)
})

## Tidy evaluation ================

test_that("Tidy evaluation", {
  # Add legends to make sure that series names are sensible
  foo <- data.frame(x=1:10,y=1:10)
  p <- arphitgg(foo, agg_aes(x=x,y=x^2)) + agg_line() + agg_legend()
  expect_true(check_graph(p, "gg-tidy-x-squared"))

  # Order by descending (#204)
  p <- arphitgg(foo, agg_aes(x=x,y=y,order=desc(y))) + agg_line()
  expect_true(check_graph(p, "gg-tidy-desc-y"))
})

## Miscellaneous ==============

test_that("Miscellaneous", {
  # Shouldn't fail if data is grouped by a variable not used in the plot (#85)
  foo <- dplyr::group_by(data.frame(x=1:10,y=rnorm(10),unused=letters[1:10]), unused)
  bar <- arphitgg(foo) + agg_line(agg_aes(x=x,y=y))
  expect_error(print(bar), NA)

  # Variables with spaces
  foo <- tibble::tibble(x=1:10, `A spaced title` = 1:10, y =  1:10)
  p <- arphitgg(foo, agg_aes(x=x,y=`A spaced title`))+agg_line()
  expect_true(check_graph(p, "gg-variables-spaces"))
  bar <- tibble::tibble(
    `spaced x` = c(1:5,1:5),
    `spaced y` = 1:10,
    `spaced group` = c("a", "a", "b", "b", "a", "a", "a", "b", "b", "b"),
    `spaced facet` = c("c", "c", "c", "c", "c", "d", "d", "d", "d", "d")
  )
  p <- arphitgg(bar, agg_aes(x = `spaced x`, y = `spaced y`, group = `spaced group`, facet = `spaced facet`)) +
    agg_line()
  expect_true(check_graph(p, "gg-variables-spaces-group-facet"))

  # NAs in the grouping variable
  foo <- data.frame(x=1:10,y=3,g=1:10)
  foo$g[4] <- NA
  p <- arphitgg(foo, agg_aes(x=x,y=y,group=g)) +
    agg_col()
  expect_true(check_graph(p, "gg-na-in-group"))

  # Group with only missings (#330)
  foo <- data.frame(x=1:10,y=1:10,group=rep(c(1:5),2))
  foo$x[c(1,6)] <- NA

  expect_error(
    arphitgg(foo, agg_aes(x=x,y=y,group=group))+agg_point(),
    NA)

  # vector comparison for long order aesthetic
  expect_warning(print(arphitgg(
    data.frame(variable = letters[1:10], y = 1:10),
    agg_aes(
      x = variable,
      y = y,
      order = case_when(
        variable == "GDP" ~ 1,
        variable == "Consumption" ~ 2,
        variable == "Dwelling" ~ 3,
        variable == "Mining" ~ 4,
        variable == "Non-mining" ~ 5,
        variable == "Public" ~ 6,
        variable == "Exports" ~ 7,
        variable == "Imports" ~ 8
      )
    )
  ) + agg_line()), NA)
})

test_that("NAs in text stuff", {
  expect_error(print(arphitgg() + agg_title(NA)), NA)
  expect_error(print(arphitgg() + agg_subtitle(NA)), NA)
  expect_error(print(arphitgg() + agg_source(NA)), NA)
  expect_error(print(arphitgg() + agg_footnote(NA)), NA)
})

test_that("rename_series", {
  p <- arphitgg(data.frame(x=1:10,y=1:10), agg_aes(x,y)) + agg_line() +
      agg_rename_series(list('Nicer series name' = 'y')) +
      agg_legend()
  expect_true(check_graph(p, "gg-rename-series-simple"))

  p <- arphitgg(data.frame(x=1:10,y=1:10,g=c(rep("a",5),rep("b",5))),
                agg_aes(x,y,g)) +
    agg_line() +
    agg_rename_series(list('Group A' = 'a', "Group B" = "b")) +
    agg_legend()
  expect_true(check_graph(p, "gg-rename-series-group"))

  p <- arphitgg(data.frame(x=1:10,y=1:10,g=c(rep("a",5),rep("b",5))),
                agg_aes(x,y,g),
                layout = "2b2") +
    agg_line(panel = 1) +
    agg_line(panel = 2) +
    agg_line(panel = 3, colour = c("red","green")) +
    agg_line(panel = 4, colour = c("red","green")) +
    agg_rename_series(list('Group A' = 'a', "Group B" = "b"), panel = c("1","3")) +
    agg_autolabel()
  expect_true(check_graph(p, "gg-rename-series-restrict-panel"))

  expect_warning(
    p <- arphitgg(data.frame(x=1:10,y=1:10), agg_aes(x,y)) + agg_line() +
    agg_rename_series(list('Nicer series name' = 'y', "foobar" = "z")) +
    agg_legend(),
    "Unable to rename series `z`"
  )
  expect_true(check_graph(p, "gg-rename-series-simple"))

})
