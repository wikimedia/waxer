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
#' @examples \dontrun{
#' wx_top_viewed_articles("en.wikipedia")
#' }
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
  project,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(access_method, granularity) %<-% wx_check_args(
    list("access_method" = access_method, "granularity" = granularity),
    args[c("access_method", "granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
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
#'   or "mobile web". If you are interested in unique devices regardless of
#'   accessed site, use "all" (default).
#' @section Access method:
#' A couple of notes about `access_method`. First, mobile apps are **_NOT_**
#' included in this dataset.
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
#' @examples \dontrun{
#' wx_unique_devices("en.wikipedia")
#' }
#' @seealso
#' - [wikitech:Research:Unique devices](https://meta.wikimedia.org/wiki/Research:Unique_devices)
#' - [wikitech:Analytics/AQS/Unique Devices](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Unique_Devices)
#' - [Introducing the unique devices dataset: a new way to estimate reach on Wikimedia projects](https://blog.wikimedia.org/2016/03/30/unique-devices-dataset/)
#' - [wikitech:Analytics/Data Lake/Traffic/Unique Devices/Last access solution](https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Unique_Devices/Last_access_solution)
#' @export
wx_unique_devices <- function(
  project,
  access_method = c("all", "desktop", "mobile web"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_method) %<-% wx_check_args(
    list("granularity" = granularity, "access_method" = access_method),
    args[c("granularity", "access_method")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
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

#' @title Project page-views
#' @inheritParams wx_top_viewed_articles
#' @param agent_type If you want to filter by agent type, use "user" or "bot".
#'   If you are interested in pageviews regardless of agent type, use "all"
#'   (default).
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`views`}{total number of page-views for the project}
#' }
#' @seealso
#' - [meta:Reseach:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#' - [wikitech:Analytics/AQS/Pageviews](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews)
#' @examples \dontrun{
#' wx_project_views("en.wikipedia")
#' }
#' @export
wx_project_views <- function(
  project,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_method, agent_type) %<-% wx_check_args(
    list("granularity" = granularity, "access_method" = access_method, "agent_type" = agent_type),
    args[c("granularity", "access_method", "agent_type")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  access_methods <- c(
    "all" = "all-access", "desktop" = "desktop",
    "mobile app" = "mobile-app", "mobile web" = "mobile-web"
  )
  agent_types <- c(
    "all" = "all-agents", "user" = "user", "bot" = "spider"
  )
  query <- wx_query_api(reqs_per_second = 100)
  path <- paste(
    "pageviews", "aggregate",
    project, access_methods[access_method], agent_types[agent_type], granularity,
    start_date, end_date,
    sep = "/"
  )
  result <- query(path)
  data_frame <- result$items %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(date = as.Date(timestamp, "%Y%m%d")) %>%
    dplyr::select(date, views)
  return(data_frame)
}

#' @title Article page-views
#' @inheritParams wx_top_viewed_articles
#' @inheritParams wx_project_views
#' @param page_name The title(s) of any article(s) in the specified project.
#'   The function takes care of replacing spaces with underscores and
#'   URI-encoding, so that non-URI-safe characters like %, / or ? are accepted
#'   -- e.g. "Are You the One?" becomes "Are_You_the_One%3F". Internally this
#'   is done with a non-exported `wx_encode_page_name` function. If you need to
#'   get the pageviews for multiple pages, you're encouraged to provide all the
#'   page names at once as this function has been optimized for that use-case.
#' @examples \dontrun{
#' wx_page_views("en.wikipedia", c("New Year's Eve", "New Year's Day"))
#' }
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`page_name`}{the `page_name` provided by the user}
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`views`}{total number of views for the page}
#' }
#' @seealso
#' - [meta:Reseach:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#' - [wikitech:Analytics/AQS/Pageviews](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews)
#' @export
wx_page_views <- function(
  project, page_name,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_method, agent_type) %<-% wx_check_args(
    list("granularity" = granularity, "access_method" = access_method, "agent_type" = agent_type),
    args[c("granularity", "access_method", "agent_type")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  access_methods <- c(
    "all" = "all-access", "desktop" = "desktop",
    "mobile app" = "mobile-app", "mobile web" = "mobile-web"
  )
  agent_types <- c(
    "all" = "all-agents", "user" = "user", "bot" = "bot"
  )
  query <- wx_query_api(reqs_per_second = 100)
  page_names <- wx_encode_page_name(page_name)
  results <- purrr::map_dfr(page_names, function(encoded_name) {
    path <- paste(
      "pageviews", "per-article",
      project, access_methods[access_method], agent_types[agent_type], encoded_name, granularity,
      start_date, end_date,
      sep = "/"
    )
    result <- query(path)
    data_frame <- result$items %>%
      purrr::map_dfr(dplyr::as_tibble) %>%
      dplyr::mutate(date = as.Date(timestamp, "%Y%m%d00")) %>%
      dplyr::select(date, views)
    return(data_frame)
  }, .id = "page_name")
  results <- results %>%
    dplyr::select(page_name, date, views) %>%
    dplyr::arrange(page_name, date)
  return(results)
}
