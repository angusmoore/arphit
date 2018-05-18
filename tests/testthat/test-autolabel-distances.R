context("Auto label distances")

# Set up
# set parameters and draw a plot so that strheight works
graphics::par(family = "sans", xaxs = "i", yaxs = "i", ps = 20, lheight = 1)
plot(1:10,1:10)

# point line distance
expect_equal(pointlinedistance(2,2,0,0,10,10), list(dist = 0, x = 2, y = 2))
d <- pointlinedistance(0,2,0,0,10,10)
expect_equal(d$x, 1)
expect_equal(d$y, 1)
w <- graphics::par("pin")[1] / (graphics::par("usr")[2] - graphics::par("usr")[1])
h <- graphics::par("pin")[2] / (graphics::par("usr")[4] - graphics::par("usr")[3])
expect_equal(d$dist, sqrt(w^2 + h^2))

# line of sight
foo <- data.frame(x=1:10,y=2)
expect_false(lineofsight(6,0,6,10,1:10,foo,c("x"),"blah"))
expect_false(lineofsight(6,0,6,10,1:10,foo,c("x","y"),"blah"))
expect_true(lineofsight(6,0,6,4,1:10,foo,c("x"),"blah"))
expect_false(lineofsight(6,0,6,4,1:10,foo,c("x","y"),"blah"))
expect_true(lineofsight(6,3,6,4,1:10,foo,c("x","y"),"blah"))

# series distance
plot(0:10,0:10)
foo <- data.frame(x=1:10,blah=0.1)
h <- 1.9 * graphics::par("pin")[2] / (graphics::par("usr")[4] - graphics::par("usr")[3])
expect_equal(series.distance(1,2,1:10,foo,c("x"),"blah",FALSE), list(distance = h, x = 1, y = 0.1))
expect_equal(series.distance(1,2,1:10,foo,c("x"),"blah",TRUE)$distance, Inf)

# shutdown
graphics.off()
