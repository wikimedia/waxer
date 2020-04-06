#' @title Top edited pages (by edits)
#' @description Lists the 100 most edited pages for a given project.
#' @param project The name of any Wikimedia project formatted like
#'   `{language code}.{project name}`, for example en.wikipedia. You may pass
#'   en.wikipedia.org and the .org will be stripped off. For projects like
#'   Wikimedia Commons (without language codes), use commons.wikimedia. For
#'   projects like www.mediawiki.org, you can use that full string, or just use
#'   mediawiki or mediawiki.org.
#' @param editor_type If you want to filter by editor-type, use one of
#'   "anonymous", "group-bot" (registered accounts belonging to the bot group),
#'   "name-bot" (registered accounts not belonging to the bot group but having
#'   bot-like names) or "user" (registered account not in bot group nor having
#'   bot-like name). If you are interested in edits regardless of their editor
#'   type, use "all-editor-types" (default).
#' @param page_type If you want to filter by page-type, use one of: "content"
#'   (edits made in content namespaces) or "non-content" (edits made in
#'   non-content namespaces). If you are interested in editors regardless of
#'   their page type, use "all" (default).
#' @param granularity The time unit for the response data. As of today,
#'   supported values are daily (default) and monthly.
#' @param start_date The date of the first day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @param end_date The date of the last day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @inheritSection wx_query_api License
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`; beginning of each month if `granularity = "monthly"`}
#'   \item{`page_name`}{page name; **NOTE**: in wikitext, the first letter
#'     of the target page is automatically capitalized}
#'  \item{`edits`}{number of edits}
#'  \item{`rank`}{`edits`-based ranking}
#' }
#' @examples
#' # Top edited articles on German Wikivoyage:
#' wx_top_edited_pages("de.wikivoyage", page_type = "content")
#' @seealso [wx_top_viewed_pages]
#' @family {content data & metrics}
#' @export
wx_top_edited_pages <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  granularity = c("daily", "monthly"),
  start_date = "20191201",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type, "granularity" = granularity),
    args[c("editor_type", "page_type", "granularity")]
  )
  c(editor_type, page_type) %<-% wx_types(editor = editor_type, page = page_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  dates <- seq(start_date, end_date, by = "day")
  query <- wx_query_api(reqs_per_second = 25)
  if (granularity == "daily") {
    results <- purrr::map_dfr(dates, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      path <- glue::glue("/edited-pages/top-by-edits/{project}/{editor_type}/{page_type}/{year}/{month}/{day}")
      result <- query(path)
      data_frame <- result$items[[1]]$results[[1]]$top %>%
        purrr::map_dfr(dplyr::as_tibble) %>%
        dplyr::mutate(date = date, project = project, page_title = wx_decode_page_name(page_title)) %>%
        dplyr::select(project, date, page_name = page_title, edits, rank) %>%
        dplyr::arrange(project, date, rank)
      return(data_frame)
    })
    return(results)
  } else {
    months <- unique(lubridate::floor_date(dates, unit = "month"))
    results <- purrr::map_dfr(months, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      day <- "all-days"
      path <- glue::glue("/edited-pages/top-by-edits/{project}/{editor_type}/{page_type}/{year}/{month}/{day}")
      tryCatch(
        {
          result <- query(path)
          data_frame <- result$items[[1]]$results[[1]]$top %>%
            purrr::map_dfr(dplyr::as_tibble) %>%
            dplyr::mutate(date = date, project = project, page_title = wx_decode_page_name(page_title)) %>%
            dplyr::select(project, date, page_name = page_title, edits, rank) %>%
            dplyr::arrange(project, date, rank)
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

#' @title New pages counts
#' @description Number of new pages for a given project and timespan.
#' @inheritParams wx_top_edited_pages
#' @inheritSection wx_newly_registered_users Granularity
#' @inheritSection wx_query_api License
#' @examples
#' # Monthly new articles on French Wiktionary:
#' wx_new_pages("fr.wiktionary", granularity = "monthly", page_type = "content")
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`}
#'   \item{`new_pages`}{Number of pages created}
#' }
#' @family {content data & metrics}
#' @export
wx_new_pages <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type, "granularity" = granularity),
    args[c("editor_type", "page_type", "granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(editor_type, page_type) %<-% wx_types(editor = editor_type, page = page_type)
  path <- paste(
    "/edited-pages/new",
    project, editor_type, page_type, granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 25)(path)
  data_frame <- result$items[[1]]$results %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(project = project, date = as.Date(lubridate::ymd_hms(timestamp))) %>%
    dplyr::select(project, date, new_pages) %>%
    dplyr::arrange(project, date)
  return(data_frame)
}

#' @title Total pages counts
#' @description Number of total pages for a given project and timespan. This
#'   is a convenience function that combines [wx_new_pages] and [base::cumsum].
#' @inheritParams wx_top_edited_pages
#' @details
#' Since there is no endpoint for this metric and it must be derived from the
#' new pages metric by taking its cumulative value, this function calls
#' [wx_new_pages] with `start_date = 2000-01-01` so that it can correctly
#' calculate cumulative number of pages from the very beginning of `project`.
#' Then it crops the results to start with the actual provided `start_date`.
#' @inheritSection wx_query_api License
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`}
#'   \item{`total_pages`}{Number of total (new + existing) pages},
#'   \item{`existing_pages`}{Number of pages already in project},
#'   \item{`new_pages`}{Number of new pages created}
#' }
#' @examples \dontrun{
#' wx_total_pages("en.wikipedia")
#' }
#' @family {content data & metrics}
#' @export
wx_total_pages <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
) {
  new_pages <- wx_new_pages(project, editor_type, page_type, granularity, "20000101", end_date)
  start_date <- as.Date(wx_format_date(start_date), "%Y%m%d")
  total_pages <- new_pages %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      project = project,
      total_pages = cumsum(new_pages),
      existing_pages = dplyr::lag(total_pages, n = 1L, default = 0)
    ) %>%
    dplyr::filter(date >= start_date) %>%
    dplyr::select(project, date, total_pages, existing_pages, new_pages) %>%
    dplyr::arrange(project, date)
  return(total_pages)
}

#' @title Edited pages counts
#' @description Number of edited pages for a given project and timespan.
#' @inheritParams wx_top_edited_pages
#' @param activity_level If you want to filter by activity-level (number of
#'   edits), use one of: "1-4", "5-24", "25-99", or "100+" edits. If you are
#'   interested in pages regardless of their activity-level, use
#'   "all" (default).
#' @inheritSection wx_query_api License
#' @examples
#' # Monthly edited pages on Hindi Wikipedia:
#' wx_edited_pages("hi.wikipedia", page_type = "content", granularity = "monthly")
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`}
#'   \item{`edited_pages`}{Number of pages which have been edited}
#' }
#' @family {content data & metrics}
#' @export
wx_edited_pages <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  activity_level = c("all", "1-4", "5-24", "25-99", "100+"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type, activity_level, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type, "activity_level" = activity_level, "granularity" = granularity),
    args[c("editor_type", "page_type", "activity_level", "granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(editor_type, page_type, activity_level) %<-% wx_types(editor = editor_type, page = page_type, activity = activity_level)
  path <- paste(
    "/edited-pages/aggregate",
    project, editor_type, page_type, activity_level, granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 25)(path)
  data_frame <- result$items[[1]]$results %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(project = project, date = as.Date(lubridate::ymd_hms(timestamp))) %>%
    dplyr::select(project, date, edited_pages) %>%
    dplyr::arrange(project, date)
  return(data_frame)
}

#' @title Project edit counts
#' @description Number of edits for a given project and timespan.
#' @inheritParams wx_top_edited_pages
#' @inheritSection wx_query_api License
#' @examples
#' # Monthly article edits on Arabic Wikipedia:
#' wx_project_edits("ar.wikipedia", page_type = "content", granularity = "monthly")
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`date`}{`Date`}
#'   \item{`edits`}{Number of edits made}
#' }
#' @family {content data & metrics}
#' @export
wx_project_edits <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type, "granularity" = granularity),
    args[c("editor_type", "page_type", "granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(editor_type, page_type) %<-% wx_types(editor = editor_type, page = page_type)
  path <- paste(
    "/edits/aggregate",
    project, editor_type, page_type, granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 25)(path)
  data_frame <- result$items[[1]]$results %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(project = project, date = as.Date(lubridate::ymd_hms(timestamp))) %>%
    dplyr::select(project, date, edits) %>%
    dplyr::arrange(project, date)
  return(data_frame)
}

#' @title Page edit counts
#' @description Number of edits for given page(s) and timespan. **Note**: Due
#'   to performance of the data-serving backend, this API endpoint limits the
#'   queryable timespan to one year (whether in daily or monthly granularity).
#' @inheritParams wx_top_edited_pages
#' @param page_name The title of any article in the specified project.
#'   The function takes care of replacing spaces with underscores and
#'   URI-encoding, so that non-URI-safe characters like %, / or ? are accepted
#'   -- e.g. "Are You the One?" becomes "Are_You_the_One%3F". Internally this
#'   is done with a non-exported `wx_encode_page_name` function. If you need to
#'   get the pageviews for multiple pages, you're encouraged to provide all the
#'   page names at once as this function has been optimized for that use-case.
#' @inheritSection wx_newly_registered_users Granularity
#' @inheritSection wx_query_api License
#' @examples \dontrun{
#' wx_page_edits(
#'   "en.wikipedia",
#'   c("Animal Crossing: New Horizons", "Animal Crossing"),
#'   granularity = "monthly",
#'   start_date = "20190101",
#'   end_date = "20191231"
#' )
#' }
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`project`}{project}
#'   \item{`page_name`}{page name}
#'   \item{`date`}{`Date`}
#'   \item{`edits`}{Number of edits made to the page}
#' }
#' @family {content data & metrics}
#' @export
wx_page_edits <- function(
  project, page_name,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "granularity" = granularity),
    args[c("editor_type", "granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  assertthat::assert_that(
    (as.Date(start_date, "%Y%m%d") + lubridate::years(1)) > as.Date(end_date, "%Y%m%d"),
    msg = "This endpoint limits the queryable timespan to one year."
  )
  c(editor_type) %<-% wx_types(editor = editor_type)
  page_names <- wx_encode_page_name(page_name)
  query <- wx_query_api(reqs_per_second = 5)
  results <- purrr::map_dfr(page_names, function(page_name) {
    path <- paste(
      "/edits/per-page",
      project, page_name, editor_type, granularity, start_date, end_date,
      sep = "/"
    )
    result <- query(path)
    data_frame <- result$items[[1]]$results %>%
      purrr::map_dfr(dplyr::as_tibble) %>%
      dplyr::mutate(date = as.Date(lubridate::ymd_hms(timestamp))) %>%
      dplyr::select(date, edits)
    return(data_frame)
  }, .id = "page_name")
  results <- results %>%
    dplyr::mutate(project = project) %>%
    dplyr::select(project, page_name, date, edits) %>%
    dplyr::arrange(project, page_name, date)
  return(results)
}
