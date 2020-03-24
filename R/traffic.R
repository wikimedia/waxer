#' @title Top viewed articles
#' @description Lists the 1000 most viewed articles for a given project and
#'   timespan (month or day).
#' @param project The name of any Wikimedia project formatted like
#'   `{language code}.{project name}`, for example en.wikipedia. You may pass
#'   en.wikipedia.org and the .org will be stripped off. For projects like
#'   Wikimedia Commons (without language codes), use commons.wikimedia. For
#'   projects like www.mediawiki.org, you can use that full string, or just use
#'   mediawiki or mediawiki.org.
#' @param access_method If you want to filter by access method, use one of:
#'   "desktop", "mobile app", or "mobile web". If you are interested in
#'   pageviews regardless of access method, use "all" (default).
#' @param granularity The time unit for the response data; "daily" by default
#'   but if you want to get the top articles of a whole month use "monthly".
#' @param start_date The date of the first day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @param end_date The date of the last day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`article`}{article name; **NOTE**: in wikitext, the first letter
#'     of the target page is automatically capitalized and spaces are
#'     represented as underscores}
#'  \item{`views`}{number of page-views (see
#'    [meta:Research:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#'    for more information on what is considered a page-view)}
#'  \item{`rank`}{`views`-based ranking}
#' }
#' @export
wx_top_viewed_articles <- function(
  project = "mediawiki",
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  # Validate "one of" arguments:
  args <- formals()
  c(access_method, granularity) %<-% wx_check_args(
    list("access_method" = access_method, "granularity" = granularity),
    args[c("access_method", "granularity")]
  )
  c(start_date, end_date) %<-% list(wx_format_date(start_date), wx_format_date(end_date))
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  assertthat::assert_that(
    end_date >= start_date,
    msg = "[User error] end_date must be same as or later than start_date"
  )
  dates <- seq(start_date, end_date, by = "day")
  access_methods <- c(
    "all" = "all-access", "desktop" = "desktop",
    "mobile app" = "mobile-app", "mobile web" = "mobile-web"
  )
  query <- wx_query_api(reqs_per_second = 100)
  if (granularity == "daily") {
    results <- purrr::map_dfr(dates, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      path <- paste(
        "pageviews", "top",
        project, access_methods[access_method], year, month, day,
        sep = "/"
      )
      result <- query(path)
      data_frame <- result$items[[1]]$articles %>%
        purrr::map_dfr(dplyr::as_tibble) %>%
        dplyr::mutate(date = date) %>%
        dplyr::select(date, dplyr::everything())
      return(data_frame)
    })
    return(results)
  } else {
    months <- unique(lubridate::floor_date(dates, unit = "month"))
    results <- purrr::map_dfr(months, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      day <- "all-days"
      path <- paste(
        "pageviews", "top",
        project, access_methods[access_method], year, month, day,
        sep = "/"
      )
      tryCatch(
        {
          result <- query(path)
          data_frame <- result$items[[1]]$articles %>%
            purrr::map_dfr(dplyr::as_tibble) %>%
            dplyr::mutate(date = date) %>%
            dplyr::select(date, dplyr::everything())
          return(data_frame)
        },
        error = function(e) {
          warning("'", path, "' returned 'Not Found' (404), likely because the date range includes current month")
        }
      )
      return(NULL)
    })
    return(results)
  }
}

#' @title Unique devices
#' @description Unique Devices dataset (v1) as developed and maintained by the
#'   Wikimedia Foundation since January 1st, 2016. This data set contains the
#'   number of unique devices having visited Wikipedia or its sisters project
#'   over some period of time. The Analytics team counts unique devices per
#'   project per day and month in a way that does not uniquely identify,
#'   fingerprint or otherwise track users.
#' @inheritParams wx_top_viewed_articles
#' @param access_method If you want to filter by accessed site, use "desktop"
#'   or "mobile". If you are interested in unique devices regardless of
#'   accessed site, use "all" (default).
#' @section Access method:
#' A couple of notes about `access_method`. First, mobile apps are **_NOT_**
#' included in this dataset. `access_method = "mobile"` refers only to number
#' of unique devices which accessed the mobile website (`*.m.*` domains).
#'
#' Second, desktop devices can (and *do*) access the mobile website, usually
#' because the user clicked on a link shared by a user from a mobile device.
#'
#' To learn more about how mobile traffic is handled by Wikimedia, refer to
#' [mw:Reading/Web/Mobile](https://www.mediawiki.org/wiki/Reading/Web/Mobile).
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`devices`}{Estimated number of unique devices.}
#'   \item{`offset`}{The number added to `underestimate` to produce `devices
#'     estimate. For more information, refer to the
#'     [Nocookie Offset section](https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Unique_Devices/Last_access_solution#Nocookie_Offset)}
#'   \item{`underestimate`}{The count produced by
#'     [WMF-Last-Access cookie counting](https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Unique_Devices/Last_access_solution#Technicalities)
#'     which, by definition, underreports users as we will not be counting
#'     users with a fresh session or users browsing without cookies.}
#' }
#' @seealso
#' - [wikitech:Research:Unique devices](https://meta.wikimedia.org/wiki/Research:Unique_devices)
#' - [wikitech:Analytics/AQS/Unique Devices](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Unique_Devices)
#' - [Introducing the unique devices dataset: a new way to estimate reach on Wikimedia projects](https://blog.wikimedia.org/2016/03/30/unique-devices-dataset/)
#' - [wikitech:Analytics/Data Lake/Traffic/Unique Devices/Last access solution](https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Unique_Devices/Last_access_solution)
#' @export
wx_unique_devices <- function(
  project = "mediawiki",
  access_method = c("all", "desktop", "mobile web"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_method) %<-% wx_check_args(
    list("granularity" = granularity, "access_method" = access_method),
    args[c("granularity", "access_method")]
  )
  c(start_date, end_date) %<-% list(wx_format_date(start_date), wx_format_date(end_date))
  assertthat::assert_that(
    end_date >= start_date,
    msg = "[User error] end_date must be same as or later than start_date"
  )
  access_methods <- c(
    "all" = "all-sites", "desktop" = "desktop-site", "mobile web" = "mobile-site"
  )
  query <- wx_query_api(reqs_per_second = 100)
  path <- paste(
    "unique-devices",
    project, access_methods[access_method], granularity, start_date, end_date,
    sep = "/"
  )
  result <- query(path)
  data_frame <- result$items %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(date = as.Date(timestamp, "%Y%m%d")) %>%
    dplyr::select(date, devices, offset, underestimate)
  return(data_frame)
}
