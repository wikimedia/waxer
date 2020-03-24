#' @title Make HTTP request and process response
#' @description Makes a request to
#'   [wikimedia.org/api/rest_v1/](https://wikimedia.org/api/rest_v1/).
#' @param reqs_per_second Maximum requests allowed per second (default 200)
#' @import ratelimitr
wx_query_api <- function(reqs_per_second = 200) {
  http_get <- function(path) {
    response <- httr::GET(
      "https://wikimedia.org", path = paste0("/api/rest_v1/metrics/", path),
      httr::user_agent("R wrapper for Wikimedia AQS API (https://github.com/bearloga/waxer)"),
      httr::accept_json()
    )
    httr::stop_for_status(response)
    if (httr::http_type(response) == "application/json") {
      result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    }
    return(result)
  }
  return(limit_rate(http_get, rate(n = reqs_per_second, period = 1)))
}
