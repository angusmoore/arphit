handleattribute <- function(data, att, default) {
  ct <- 1
  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    if (is.null(s$attributes[[att]])) {
      # Give it a default
      data$series[[i]]$attributes[[att]]<- default[ct]
      ct <- ct %% length(default) + 1
    }
  }
  return(data)
}

handleattributes <- function(data) {
  for (p in names(data)) {
    if (is.null(getOption("arphit.user_colors"))) {
      data[[p]] <- handleattribute(data[[p]], "col", DEFAULTCOLORS)
    } else {
      data[[p]] <- handleattribute(data[[p]], "col", getOption("arphit.user_colors"))
    }
  	data[[p]] <- handleattribute(data[[p]], "pch", DEFAULTPCH)
  	data[[p]] <- handleattribute(data[[p]], "lty", DEFAULTLTY)
  	data[[p]] <- handleattribute(data[[p]], "lwd", DEFAULTLWD)
  	data[[p]] <- handleattribute(data[[p]], "barcol", DEFAULTBARCOL)
  	data[[p]] <- handleattribute(data[[p]], "pointsize", 1)
  }
  return(data)
}
