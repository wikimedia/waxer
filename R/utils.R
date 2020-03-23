wx_extract_ymd <- function(x) {
  y <- lubridate::year(x)
  m <- lubridate::month(x)
  d <- lubridate::mday(x)
  assertthat::assert_that(
    length(x) >= 1,
    msg = "[Developer error] must provide at least one date"
  )
  if (length(x) == 1) {
    return(list(y, m, d))
  } else {
    return(dplyr::tibble(year = y, month = m, day = d))
  }
}

wx_extract_yyyymmdd <- function(x) {
  c(year, month, day) %<-% wx_extract_ymd(x)
  return(list(year, sprintf("%02.0f", month), sprintf("%02.0f", day)))
}

wx_format_date <- function(x) {
  assertthat::assert_that(length(x) == 1)
  if (any(c("POSIXt", "Date") %in% class(x))) {
    formatted_date <- format(x, "%Y%m%d")
  } else if ("character" %in% class(x)) {
    assertthat::assert_that(grepl("^[0-9]{8}$", x))
    formatted_date <- x
  } else {
    stop("'date' (", x, ") is not valid")
  }
  return(formatted_date)
}

wx_param_mismatch_error_msg <- function(val, opts, name) {
  vals <- paste0(opts, collapse = "', '")
  return(glue::glue("[User error] Value of {name} ('{val}') not one of '{vals}'"))
}

wx_check_args <- function(values, params) {
  assertthat::assert_that(
    length(values) == length(params),
    msg = "[Developer error] Mismatched number of elements in check_args"
  )
  assertthat::assert_that(
    all(names(values) == names(params)),
    msg = "[Developer error] Mismatched values and parameters in check_args"
  )
  for (param in names(params)) {
    acceptable <- eval(params[[param]])
    assertthat::assert_that(
      values[[param]][1] %in% acceptable,
      msg = wx_param_mismatch_error_msg(values[[param]][1], acceptable, param)
    )
  }
  return(purrr::map(values, ~ .x[1]))
}
