
# Labels
foo <- list(list(x = 10, y = -1, text = "Hi", panel = 1, color = "black"))
foo2 <- list(list(x = 10, y = -1, text = "Hi", panel = 1, color = "black"), list(x = 15, y = 10, text = "Bye", panel = 1, color = "green"))
shoulderror <- list(list(x = 10, text = "Hi", panel = 1, color = "black"))
shoulderror2 <- list(list(x = 10, y = -1, text = "Hi", panel = 1, color = "black"), list(x = 15, text = "Bye", panel = 1, color = "green"))

nocolor <- list(list(x = 10, y = -1, text = "Hi", panel = 1))
nocolor2 <- list(list(x = 10, y = -1, text = "Hi", panel = 1), list(x = 15, y = 10, text = "Bye", panel = 1, color = "green"))

expect_that(sanitychecklabels(foo), is_identical_to(foo))
expect_that(sanitychecklabels(foo2), is_identical_to(foo2))
expect_that(sanitychecklabels(nocolor), is_identical_to(foo))
expect_that(sanitychecklabels(nocolor2), is_identical_to(foo2))

expect_error(sanitychecklabels(shoulderror))
expect_error(sanitychecklabels(shoulderror2))

# Arrows
foo <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1, lwd = 1, color = "black"))
foo2 <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1, lwd = 1, color = "black"), list(tail.x = 8, tail.y = 0, head.x = 9, head.y = 3, panel = 1, lwd = 1, color = "red"))
shoulderror <- list(list(tail.x = 10, head.x = 12, head.y = 0, panel = 1, lwd = 1, color = "black"))
shoulderror2 <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1, lwd = 1, color = "black"), list(tail.x = 8, head.x = 9, head.y = 3, panel = 1, lwd = 1, color = "red"))
nocolor <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1, lwd = 1))
nocolor2 <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1, lwd = 1), list(tail.x = 8, tail.y = 0, head.x = 9, head.y = 3, panel = 1, lwd = 1, color = "red"))
nolwdnocol <- list(list(tail.x = 10, tail.y = -1, head.x = 12, head.y = 0, panel = 1))

expect_that(sanitycheckarrows(foo), is_identical_to(foo))
expect_that(sanitycheckarrows(foo2), is_identical_to(foo2))
expect_that(sanitycheckarrows(nocolor), is_identical_to(foo))
expect_that(sanitycheckarrows(nocolor2), is_identical_to(foo2))
expect_that(sanitycheckarrows(nolwdnocol), is_identical_to(foo))

expect_error(sanitycheckarrows(shoulderror))
expect_error(sanitycheckarrows(shoulderror2))

# Background shading
foo <- list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 1, panel = 1, color = "lightgrey"))
foo2 <- list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 1, panel = 1, color = "lightgrey"), list(x1 = 10, y1 = -1, x2 = 12, y2 = 1, panel = 1, color = "green"))
expect_that(sanitycheckbgshading(foo), is_identical_to(foo))
expect_that(sanitycheckbgshading(foo2), is_identical_to(foo2))

shoulderror <- list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 1, color = "lightgrey"))
expect_error(sanitycheckbgshading(shoulderror))

nocolor <- list(list(x1 = NA, y1 = -1, x2 = NA, y2 = 1, panel = 1))
expect_that(sanitycheckbgshading(nocolor), is_identical_to(foo))

nox <- list(list(y1 = -1, y2 = 1, panel = 1, color = "lightgrey"))
expect_that(sanitycheckbgshading(nox), is_identical_to(list(list(y1 = -1, y2 = 1, panel = 1, color = "lightgrey", x1 = NA, x2 = NA))))

vline <- list(list(x = 2001, panel = 1, color = "green", lty = 2))
twovline <- list(list(x = 2001, panel = 1, color = "green", lty = 2), list(x = 2002, panel = 1, color = "red", lty = 3))
hline <- list(list(y = -0.5, panel = 1))
specificline <- list(list(x1 = 2000, y1 = -1, x2 = 2001, y2 = 0, panel = 1))
nopanel <- list(list(x = 2001))
specificerror <- list(list(x1 = 2000, y1 = -1, y2 = 0, panel = 1))

expect_that(sanitychecklines(vline), is_identical_to(list(list(x = 2001, panel = 1, color = "green", lty = 2, x1 = 2001, x2 = 2001, y1 = NA, y2 = NA))))
expect_that(sanitychecklines(hline), is_identical_to(list(list(y = -0.5, panel = 1, x1 = NA, x2 = NA, y1 = -0.5, y2 = -0.5, color = "black", lty = 1))))
expect_that(sanitychecklines(specificline), is_identical_to(list(list(x1 = 2000, y1 = -1, x2 = 2001, y2 = 0, panel = 1, color = "black", lty = 1))))
expect_that(sanitychecklines(twovline), is_identical_to(list(list(x = 2001, panel = 1, color = "green", lty = 2, x1 = 2001, x2 = 2001, y1 = NA, y2 = NA), list(x = 2002, panel = 1, color = "red", lty = 3, x1 = 2002, x2 = 2002, y1 = NA, y2 = NA))))
expect_error(sanitychecklines(nopanel))
expect_error(sanitychecklines(specificerror))
