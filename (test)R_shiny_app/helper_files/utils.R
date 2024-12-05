# Utility functions
format_date_range <- function(date_range) {
  paste(format(date_range[1], "%Y-%m-%d"), "to",
        format(date_range[2], "%Y-%m-%d"))
}

calculate_summary_stats <- function(data, variable) {
  data %>%
    summarise(
      mean = mean(.data[[variable]], na.rm = TRUE),
      sd = sd(.data[[variable]], na.rm = TRUE),
      min = min(.data[[variable]], na.rm = TRUE),
      max = max(.data[[variable]], na.rm = TRUE)
    )
}

validate_date_range <- function(date_range, data) {
  if (is.null(date_range)) {
    return(NULL)
  }
  
  min_date <- min(data$Year, na.rm = TRUE)
  max_date <- max(data$Year, na.rm = TRUE)
  
  list(
    start = max(date_range[1], min_date),
    end = min(date_range[2], max_date)
  )
}

