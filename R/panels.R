maxpanels <- function(layout) {
  # Switch through the layout options
  if (layout == "1" || layout == "2v") {
    maxnp <- 2
  } else if (layout == "1h") {
    maxnp <- 1
  } else if (layout == "2h" || layout == "2b2") {
    maxnp <- 4
  } else if (layout == "3v") {
    maxnp <- 3
  } else if (layout == "3h" || layout == "3b2") {
    maxnp <- 6
  } else if (layout == "4h" || layout == "4b2") {
    maxnp <- 8
  } else {
    stop(paste0("Unknown layout option ", layout,
                ". Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2."),
         call. = FALSE)
  }


  maxnp
}

permitted_panels <- function(layout) {
  maxnp <- maxpanels(layout)
  permittedpanels <- as.character(1:maxnp)

  permittedpanels
}

getlocation <- function(p, layout) {
  if (layout == "1" || layout == "1h") {
    c(1, 1)
  } else if (layout == "2h") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 1),
      "3" = c(2, 1),
      "4" = c(2, 1)
    )
  } else if (layout == "2v") {
    switch(p,
           "1" = c(1, 1),
           "2" = c(1, 2))
  } else if (layout == "2b2") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 2),
      "3" = c(2, 1),
      "4" = c(2, 2)
    )
  } else if (layout == "3v") {
    switch(p,
           "1" = c(1, 1),
           "2" = c(1, 2),
           "3" = c(1, 3))
  } else if (layout == "3h") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 1),
      "3" = c(2, 1),
      "4" = c(2, 1),
      "5" = c(3, 1),
      "6" = c(3, 1)
    )
  } else if (layout == "3b2") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 2),
      "3" = c(2, 1),
      "4" = c(2, 2),
      "5" = c(3, 1),
      "6" = c(3, 2)
    )
  } else if (layout == "4h") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 1),
      "3" = c(2, 1),
      "4" = c(2, 1),
      "5" = c(3, 1),
      "6" = c(3, 1),
      "7" = c(4, 1),
      "8" = c(4, 1)
    )
  } else if (layout == "4b2") {
    switch(
      p,
      "1" = c(1, 1),
      "2" = c(1, 2),
      "3" = c(2, 1),
      "4" = c(2, 2),
      "5" = c(3, 1),
      "6" = c(3, 2),
      "7" = c(4, 1),
      "8" = c(4, 2)
    )
  } else {
    stop(
      paste0(
        "Unknown layout option ",
        layout,
        ". Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2."
      ),
      call. = FALSE
    )
  }
}

getsides <- function(p, layout) {
  if (layout == "1" || layout == "2v") {
    if (p == "1") {
      side <- 2
    } else if (p == "2") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "1h") {
    if (p == "1") {
      side <- 1
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "2h" || layout == "2b2") {
    if (p == "1" || p == "3") {
      side <- 2
    } else if (p == "2" || p == "4") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "3v") {
    if (p == "1") {
      side <- 2
    } else if (p == "2") {
      side <- NA
    } else if (p == "3") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "3h" || layout == "3b2") {
    if (p == "1" || p == "3" || p == "5") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "4h" || layout == "4b2") {
    if (p == "1" || p == "3" || p == "5" || p == "7") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6" || p == "8") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else  {
    stop(paste0("Unknown layout option ", layout,
                ". Options are 1, 1h, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2."),
         call. = FALSE)
  }


  side
}

dropfirstxlabel <- function(p, layout, dropxlabel, ts, at, label, xlim) {
  if ((layout == "2v" && p == "2") ||
      (layout == "2b2" && p == "4") ||
      (layout == "3v" && (p == "2" || p == "3")) ||
      (layout == "3b2" && p == "6") ||
      (layout == "4b2" && p == "8")) {
    if (is.na(dropxlabel)) {
      if (ts) {
        label_left_border <- at - 0.5 * getstrwidth(label, units = "user")
        overlap <- (label_left_border - xlim[1]) / (xlim[2] - xlim[1])
        # allow labels to overlap up to 1.5 per cent. the last year margin is
        # 3% so they should not overlap
        return(overlap < -0.015)
      } else {
        return(FALSE)
      }
    } else {
      return(dropxlabel)
    }
  } else {
    return(FALSE)
  }
}

needxlabels <- function(p, layout) {
  if (layout %in% c("1", "1h")) {
    p == "1"
  } else if (layout == "2h") {
    p == "3"
  } else if (layout == "2v" || layout == "3v") {
    TRUE
  } else if (layout == "2b2") {
    p == "3" || p == "4"
  } else if (layout == "3b2") {
    p == "5" || p == "6"
  } else if (layout == "3h") {
    p == "5"
  } else if (layout == "4h") {
    p == "7"
  } else if (layout == "4b2") {
    p == "7" || p == "8"
  } else {
    stop(paste("Unknown layout option ", layout,
               ". Options are 1, 1h, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2.",
               sep = ""),
         call. = FALSE)
  }
}

dropbottomlabel <- function(p, layout) {
  if (layout %in% c("1", "1h", "2v", "3v")) {
    FALSE
  } else if (layout %in% c("2h", "2b2")) {
    p %in% c("1", "2")
  } else if (layout %in% c("3h", "3b2")) {
    p %in% c("1", "2", "3", "4")
  } else if (layout %in% c("4h", "4b2")) {
    p %in% c("1", "2", "3", "4", "5", "6")
  } else {
    stop(paste("Unknown layout option ", layout,
               ". Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2.",
               sep = ""),
         call. = FALSE)
  }
}

needgrid <- function(p, layout) {
  if (layout == "1" || layout == "1h") {
    p == "1"
  } else if (layout == "2h") {
    p == "1" || p == "3"
  } else if (layout == "3h") {
    p == "1" || p == "3" || p == "5"
  } else if (layout == "4h") {
    p == "1" || p == "3" || p == "5" || p == "7"
  } else if (layout == "2v" || layout == "2b2" || layout == "3v" ||
             layout == "3b2" || layout == "4b2") {
    TRUE
  } else {
    stop(paste0("Unknown layout option ", layout,
                ". Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2."),
         call. = FALSE)
  }
}

tickadjustment <- function(layout) {
  switch(layout,
         "1" = 1,
         "1h" = 1,
         "2h" = 2,
         "2v" = 3 / 2,
         "2b2" = 2,
         "3h" = 3,
         "3v" = 2,
         "3b2" = 3,
         "4h" = 4,
         "4b2" = 4)
}
