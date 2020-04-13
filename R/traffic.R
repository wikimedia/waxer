#' @title Top viewed pages
#' @description Lists the 1000 most viewed pages for a given project and
#'   timespan (month or day).
#' @inheritParams wx_top_edited_pages
#' @param access_method If you want to filter by access method, use one of:
#'   "desktop", "mobile app", or "mobile web". If you are interested in
#'   pageviews regardless of access method, use "all" (default).
#' @inheritSection wx_query_api License
#' @examples
#' wx_top_viewed_pages("en.wikipedia")
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`project`}{project}
#'   \item{`page_name`}{page name; **NOTE**: in wikitext, the first letter
#'     of the target page is automatically capitalized}
#'  \item{`views`}{number of page-views (see
#'    [meta:Research:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#'    for more information on what is considered a page-view)}
#'  \item{`rank`}{`views`-based ranking}
#' }
#' @family {traffic data & metrics}
#' @export
wx_top_viewed_pages <- function(
  project,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(access_method, granularity) %<-% wx_check_args(
    list("access_method" = access_method, "granularity" = granularity),
    args[c("access_method", "granularity")]
  )
  c(access_method) %<-% wx_types(access = access_method)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  dates <- seq(start_date, end_date, by = "day")
  query <- wx_query_api(reqs_per_second = 100)
  if (granularity == "daily") {
    results <- purrr::map_dfr(dates, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      path <- paste(
        "/pageviews/top",
        project, access_method, year, month, day,
        sep = "/"
      )
      result <- query(path)
      data_frame <- result$items[[1]]$articles %>%
        purrr::map_dfr(dplyr::as_tibble) %>%
        dplyr::mutate(date = date, project = project, article = wx_decode_page_name(article)) %>%
        dplyr::select(date, project, page_name = article, views, rank) %>%
        dplyr::arrange(date, rank)
      return(data_frame)
    })
    return(results)
  } else {
    months <- unique(lubridate::floor_date(dates, unit = "month"))
    results <- purrr::map_dfr(months, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      day <- "all-days"
      path <- paste(
        "/pageviews/top",
        project, access_method, year, month, day,
        sep = "/"
      )
      tryCatch(
        {
          result <- query(path)
          data_frame <- result$items[[1]]$articles %>%
            purrr::map_dfr(dplyr::as_tibble) %>%
            dplyr::mutate(date = date, project = project, article = wx_decode_page_name(article)) %>%
            dplyr::select(date, project, page_name = article, views, rank) %>%
            dplyr::arrange(date, rank)
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

#' @title Unique device counts
#' @description Unique Devices dataset (v1) as developed and maintained by the
#'   Wikimedia Foundation since January 1st, 2016. This data set contains the
#'   number of unique devices having visited Wikipedia or its sisters project
#'   over some period of time. The Analytics team counts unique devices per
#'   project per day and month in a way that does not uniquely identify,
#'   fingerprint or otherwise track users.
#' @inheritParams wx_top_edited_pages
#' @param access_site If you want to filter by accessed site, use "desktop"
#'   or "mobile". If you are interested in unique devices regardless of
#'   accessed site, use "all" (default). **_NOT_** to be confused with
#'   `access_method` parameter in other functions.
#' @section Site:
#' Desktop devices can (and *do*) access the mobile website, usually because
#' the user clicked on a link shared by a user from a mobile device. So do not
#' use this metric as a proxy for mobile vs desktop devices.
#'
#' To learn more about how mobile traffic is handled by Wikimedia, refer to
#' [mw:Reading/Web/Mobile](https://www.mediawiki.org/wiki/Reading/Web/Mobile).
#' @inheritSection wx_query_api License
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`devices`}{Estimated number of unique devices.}
#'   \item{`offset`}{The number added to `underestimate` to produce `devices` estimate.}
#'   \item{`underestimate`}{The count produced by WMF-Last-Access cookie counting, which, by definition, underreports users as we will not be counting users with a fresh session or users browsing without cookies.}
#' }
#' @examples
#' wx_unique_devices("en.wikipedia", access_site = "mobile", granularity = "monthly")
#' @seealso
#' - [wikitech:Research:Unique devices](https://meta.wikimedia.org/wiki/Research:Unique_devices)
#' - [wikitech:Analytics/AQS/Unique Devices](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Unique_Devices)
#' - [Introducing the unique devices dataset: a new way to estimate reach on Wikimedia projects](https://blog.wikimedia.org/2016/03/30/unique-devices-dataset/)
#' - [wikitech:Analytics/Data Lake/Traffic/Unique Devices/Last access solution](https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Unique_Devices/Last_access_solution)
#' @family {traffic data & metrics}
#' @export
wx_unique_devices <- function(
  project,
  access_site = c("all", "desktop", "mobile"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_site) %<-% wx_check_args(
    list("granularity" = granularity, "access_site" = access_site),
    args[c("granularity", "access_site")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  access_sites <- c("all" = "all-sites", "desktop" = "desktop-site", "mobile" = "mobile-site")
  path <- paste(
    "/unique-devices",
    project, access_sites[access_site], granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 100)(path)
  data_frame <- result$items %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(project = project, date = as.Date(timestamp, "%Y%m%d")) %>%
    dplyr::select(project, date, devices, offset, underestimate) %>%
    dplyr::arrange(project, date)
  return(data_frame)
}

#' @title Project page-view counts
#' @inheritParams wx_top_edited_pages
#' @inheritParams wx_top_viewed_pages
#' @param agent_type If you want to filter by agent type, use "user" or "bot".
#'   If you are interested in pageviews regardless of agent type, use "all"
#'   (default).
#' @param granularity The time unit for the response data. As of today,
#'   supported values are daily (default), monthly, and hourly. In case of
#'   hourly granularity, for simplicity, the function retrieves data from the
#'   very start of `start_date` (hour 00) to the very end of `end_date` (hour
#'   23).
#' @inheritSection wx_query_api License
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date` if daily or monthly granularity, `POSIXct` for hourly; all times are in UTC}
#'   \item{`views`}{total number of page-views for the project}
#' }
#' @seealso
#' - [meta:Reseach:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#' - [wikitech:Analytics/AQS/Pageviews](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews)
#' @examples
#' # block from 2017-04-29 to 2020-01-15
#' wx_project_views(
#'   "tr.wikipedia", # Turkish
#'   granularity = "monthly",
#'   start_date = "20191001",
#'   end_date = "20200229"
#' )
#' @family {traffic data & metrics}
#' @export
wx_project_views <- function(
  project,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly", "hourly"),
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
  c(access_method, agent_type) %<-% wx_types(access = access_method, agent = agent_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  if (granularity == "hourly") {
    path <- paste(
      "/pageviews/aggregate",
      project, access_method, agent_type, granularity,
      paste0(start_date, "00"), paste0(end_date, "23"),
      sep = "/"
    )
  } else {
    path <- paste(
      "/pageviews/aggregate",
      project, access_method, agent_type, granularity, start_date, end_date,
      sep = "/"
    )
  }
  result <- wx_query_api(reqs_per_second = 100)(path)
  data_frame <- result$items %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(project = project)
  if (granularity == "hourly") {
    data_frame <- data_frame %>%
      dplyr::mutate(time = lubridate::ymd_h(timestamp)) %>%
      dplyr::select(project, time, views) %>%
      dplyr::arrange(project, time)
  } else {
    data_frame <- data_frame %>%
      dplyr::mutate(date = as.Date(timestamp, "%Y%m%d00")) %>%
      dplyr::select(project, date, views) %>%
      dplyr::arrange(project, date)
  }
  return(data_frame)
}

#' @title Page view counts
#' @inheritParams wx_top_edited_pages
#' @inheritParams wx_top_viewed_pages
#' @inheritParams wx_project_views
#' @inheritParams wx_page_edits
#' @inheritSection wx_query_api License
#' @examples
#' wx_page_views(
#'   "en.wikipedia",
#'   c("New Year's Eve", "New Year's Day"),
#'   start_date = "20191231",
#'   end_date = "20200101"
#' )
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`page_name`}{the `page_name` provided by the user}
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`views`}{total number of views for the page}
#' }
#' @seealso
#' - [meta:Reseach:Page view](https://meta.wikimedia.org/wiki/Research:Page_view)
#' - [wikitech:Analytics/AQS/Pageviews](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews)
#' @family {traffic data & metrics}
#' @export
wx_page_views <- function(
  project, page_name,
  access_method = c("all", "desktop", "mobile web", "mobile app"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, access_method, agent_type) %<-% wx_check_args(
    list("granularity" = granularity, "access_method" = access_method, "agent_type" = agent_type),
    args[c("granularity", "access_method", "agent_type")]
  )
  c(access_method, agent_type) %<-% wx_types(access = access_method, agent = agent_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  query <- wx_query_api(reqs_per_second = 100)
  page_names <- wx_encode_page_name(page_name)
  results <- purrr::map_dfr(page_names, function(encoded_name) {
    path <- paste(
      "/pageviews/per-article",
      project, access_method, agent_type, encoded_name, granularity, start_date, end_date,
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
    dplyr::mutate(project = project) %>%
    dplyr::select(project, page_name, date, views) %>%
    dplyr::arrange(project, page_name, date)
  return(results)
}
