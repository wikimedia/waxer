#' @title Top editors by edits
#' @description Top 100 editors per day by number of edits. **Caution**:
#'   anonymous editors IPs are replaced by `NA`.
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
#'   their page type, use "all-page-types" (default).
#' @param start_date The date of the first day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @param end_date The date of the last day to include, in YYYYMMDD format.
#'   Can also be a `Date` or a `POSIXt` object, which will be auto-formatted.
#' @export
wx_top_editors <- function(
  project = "mediawiki",
  editor_type = c("all-editor-types", "anonymous", "user", "name-bot", "group-bot"),
  page_type = c("all-page-types", "content", "non-content"),
  start_date = "20191231",
  end_date = "20200101"
) {
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type),
    args[c("editor_type", "page_type")]
  )
  c(start_date, end_date) %<-% list(wx_format_date(start_date), wx_format_date(end_date))
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  assertthat::assert_that(
    end_date >= start_date,
    msg = "[User error] end_date must be same as or later than start_date"
  )
  dates <- seq(start_date, end_date, by = "day")
  query <- wx_query_api(reqs_per_second = 25)
  results <- purrr::map_dfr(dates, function(date) {
    c(year, month, day) %<-% wx_extract_yyyymmdd(date)
    path <- paste(
      "editors", "top-by-edits",
      project, editor_type, page_type, year, month, day,
      sep = "/"
    )
    result <- query(path)
    return(result)
  })
  return(tidyr::unnest_wider(results, top))
}

#' @title Active editors
#' @description Number of active editors. **Caution**: This metric measures
#'   editors' activity, therefore it includes edits on redirects.
#' @inheritParams wx_top_editors
#' @param activity_level If you want to filter by activity-level (number of
#'   edits), use one of: "1-4", "5-24", "25-99", or "100+" edits. If you are
#'   interested in editors regardless of their activity-level, use
#'   "all-activity-levels" (default).
#' @param granularity The time unit for the response data. As of today,
#'   supported values are daily and monthly.
#' @section Granularity and dates:
#' For "monthly" `granularity`, the `start_date` and `end_date` need to contain
#' the full month.
#'
#' For example, `start_date = "20191101"` and
#' `end_date = "20191231"` would only give the monthly total for 2019-11. To
#' include 2019-12, use `end_date = "20200101"`.
#'
#' Frustratingly, `start_date = "20191201"` and `end_date = "20191231"` does
#' **_not_** yield 2019-12 monthly total. Use `end_date = "20200101"` for that.
#' @seealso [wikitech\:Analytics/AQS/Wikistats 2#Editors](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Wikistats_2#Editors)
#' @export
wx_active_editors <- function(
  project = "mediawiki",
  editor_type = c("all-editor-types", "anonymous", "user", "name-bot", "group-bot"),
  page_type = c("all-page-types", "content", "non-content"),
  activity_level = c("all-activity-levels", "1-4", "5-24", "25-99", "100+"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
) {
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type, activity_level, granularity) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type, "activity_level" = activity_level, "granularity" = granularity),
    args[c("editor_type", "page_type", "activity_level", "granularity")]
  )
  c(start_date, end_date) %<-% list(wx_format_date(start_date), wx_format_date(end_date))
  activity_levels <- c(
    "all-activity-levels" = "all-activity-levels",
    "1-4" = "1..4-edits", "5-24" = "5..24-edits",
    "25-99" = "25..99-edits", "100+" = "100..-edits"
  )
  path <- paste(
    "editors", "aggregate",
    project, editor_type, page_type, activity_levels[activity_level],
    granularity, start_date, end_date,
    sep = "/"
  )
  results <- wx_query_api(reqs_per_second = 25)(path)
  return(results)
}
