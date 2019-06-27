create_data_panel <- function(panel, data) {
  df <- data.frame(agg_x_variable = panel$x, stringsAsFactors = FALSE)
  for (i in seq_along(panel$series)) {
    newdf <- data.frame(agg_x_variable = series_x_values(panel, i),
                        y = series_values(panel, i),
                        stringsAsFactors = FALSE)
    names(newdf) <- c("agg_x_variable", panel$series[[i]]$name)
    df <- dplyr::full_join(df, newdf, by = "agg_x_variable")
  }
  colnames(df)[1] <- ""
  return(df)
}

restrict_xlim_xlsx <- function(panel, data, xlim) {
  if (is.null(xlim)) xlim <- c(-Inf, Inf)
  if (is.na(xlim[1])) xlim[1] <- -Inf
  if (is.na(xlim[2])) xlim[2] <- Inf

  indices <- get_x_plot_locations(panel[[1]], data)

  if (lubridate::is.Date(indices) || lubridate::is.POSIXt(indices)) {
    freq <- frequencyof(indices)
    indices <- make_decimal_date(indices, freq)
  }

  keep <- indices >= xlim[1] & indices <= xlim[2]

  panel[keep, ]
}

write_to_excel <- function(gg, filename) {
  metadata_names <- c("Title", "Subtitle", "Footnotes", "Sources")
  metadata_values <- c(ifelse(is.null(gg$title), "", gg$title),
                       ifelse(is.null(gg$subtitle), "", gg$subtitle),
                       paste(gg$footnotes, collapse = "; "),
                       paste(gg$sources, collapse = "; "))
  metadata <-
    data.frame(
      Metadata = metadata_names,
      Value = metadata_values,
      stringsAsFactors = FALSE
    )

  panels <- lapply(gg$data, create_data_panel)
  data <- convert_ts_to_decimal_date(gg$data)

  # Handle applying x lim to all axes
  if (!is.list(gg$xlim)) gg$xlim <- lapply(panels, function(x) gg$xlim)

  for (p in names(panels)) {
    panels[[p]] <- restrict_xlim_xlsx(panels[[p]], data[[p]], gg$xlim[[p]])
  }

  writexl::write_xlsx(append(list(Meta = metadata), panels),
                      path = filename,
                      col_names = TRUE,
                      format_headers = TRUE)
}
