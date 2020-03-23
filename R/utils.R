wx_format_date <- function(date) {
  assertthat::assert_that(length(date) == 1)
  if (any(c("POSIXt", "Date") %in% class(date))) {
    formatted_date <- format(date, "%Y%m%d")
  } else if ("string" %in% class(date)) {
    assertthat::assert_that(grepl("^[0-9]{8}$", date))
    formatted_date <- date
  } else {
    stop("'date' (", date, ") is not valid")
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
