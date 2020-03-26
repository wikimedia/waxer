#' @title Generate a rate-limited API caller
#' @description Generates a function which makes a request to
#'   [wikimedia.org/api/rest_v1/](https://wikimedia.org/api/rest_v1/). This
#'   generator is primarily intended for internal use, but can theoretically be
#'   used by others to write custom Wikimedia REST API queries.
#' @param reqs_per_second maximum requests allowed per second (default 200)
#' @section License:
#' Data retrieved from the API endpoint is available under the
#' [CC0 1.0 license](https://creativecommons.org/publicdomain/zero/1.0/).
#' @return A rate-limited function with parameters:
#' \describe{
#'   \item{`path`}{path after endpoint; e.g.
#'     `/registered-users/new/{project}/{granularity}/{start}/{end}`}
#'   \item{`endpoint`}{`/metrics` by default but can be changed to `/feed` (for example)}
#' }
#' The generated function, when run, processed the JSON response and returns a
#' list. It is up to the user of this generator to turn the results into a data
#' frame or another object.
#' @import ratelimitr
wx_query_api <- function(reqs_per_second = 200) {
  http_get <- function(path, endpoint = "/metrics") {
    response <- httr::GET(
      "https://wikimedia.org", path = paste0("/api/rest_v1", endpoint, path),
      httr::user_agent("R wrapper for Wikimedia AQS API (https://github.com/bearloga/waxer)"),
      httr::accept_json()
    )
    httr::stop_for_status(response)
    if (httr::http_type(response) == "application/json") {
      result <- jsonlite::fromJSON(
        httr::content(response, as = "text", encoding = "UTF-8"),
        simplifyVector = FALSE
      )
    }
    return(result)
  }
  return(limit_rate(http_get, rate(n = reqs_per_second, period = 1)))
}
