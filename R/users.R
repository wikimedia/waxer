#' @title Top editors by edit count
#' @description Top 100 editors per day by number of edits. **Caution**:
#'   anonymous editors IPs are replaced by `NA`.
#' @inheritParams wx_top_edited_pages
#' @inheritSection wx_query_api License
#' @return A tibble data frame with the following columns:
#' \describe{
#'   \item{`date`}{`Date`}
#'   \item{`user_text`}{the editor's username; `NA` if anonymous (instead of IP address)}
#'   \item{`edits`}{number of edits the user made on `date`}
#'   \item{`rank`}{the rank based on `edits`}
#' }
#' @examples \dontrun{
#' wx_top_editors(
#'   "commons.wikimedia",
#'   editor_type = "bot",
#'   page_type = "content",
#'   start_date = "20191001",
#'   end_date = "20191031"
#' )
#' }
#' @family {data & metrics on users}
#' @export
wx_top_editors <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  start_date = "20191231",
  end_date = "20200101"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(editor_type, page_type) %<-% wx_check_args(
    list("editor_type" = editor_type, "page_type" = page_type),
    args[c("editor_type", "page_type")]
  )
  c(editor_type, page_type) %<-% wx_types(editor = editor_type, page = page_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  dates <- seq(start_date, end_date, by = "day")
  query <- wx_query_api(reqs_per_second = 25)
  results <- purrr::map_dfr(dates, function(date) {
    c(year, month, day) %<-% wx_extract_yyyymmdd(date)
    path <- paste(
      "editors/top-by-edits",
      project, editor_type, page_type, year, month, day,
      sep = "/"
    )
    result <- query(path)
    # Tidy up results:
    data_frame <- result$items[[1]]$results %>%
      purrr::map_dfr(dplyr::as_tibble) %>%
      dplyr::mutate(
        timestamp = lubridate::ymd_hms(timestamp),
        date = as.Date(timestamp)
      ) %>%
      dplyr::select(-timestamp) %>%
      dplyr::select(date, dplyr::everything())
    return(data_frame)
  })
  return(tidyr::unnest_wider(results, top))
}

#' @title Active editors counts
#' @description Number of active editors. **Caution**: This metric measures
#'   editors' activity, therefore it includes edits on redirects.
#' @inheritParams wx_top_edited_pages
#' @param activity_level If you want to filter by activity-level (number of
#'   edits), use one of: "1-4", "5-24", "25-99", or "100+" edits. If you are
#'   interested in editors regardless of their activity-level, use
#'   "all" (default).
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
#' @inheritSection wx_query_api License
#' @return A tibble data frame with columns `date` and `editors`.
#' @seealso [wikitech:Analytics/AQS/Wikistats 2](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Wikistats_2)
#' @examples
#' wx_active_editors(
#'   "commons.wikimedia",
#'   editor_type = "bot",
#'   page_type = "content",
#'   activity_level = "100+",
#'   granularity = "monthly",
#'   start_date = "20200101",
#'   end_date = "20200301"
#' )
#' @family {data & metrics on users}
#' @export
wx_active_editors <- function(
  project,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  page_type = c("all", "content", "non-content"),
  activity_level = c("all", "1-4", "5-24", "25-99", "100+"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
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
    "/editors/aggregate",
    project, editor_type, page_type, activity_level, granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 25)(path)
  # Tidy up results:
  data_frame <- result$items[[1]]$results %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(date = as.Date(lubridate::ymd_hms(timestamp))) %>%
    dplyr::select(date, editors)
  return(data_frame)
}

#' @title Newly registered users counts
#' @description The count of users having registered themselves. This metric
#'   excludes user-account created automatically through the auto-login
#'   system, and accounts created by peers.
#' @inheritParams wx_top_edited_pages
#' @details
#' Thanks to [CentralAuth](https://www.mediawiki.org/wiki/Extension:CentralAuth),
#' we now are sure that the same usernames in different wiki-projects belong to
#' the same user, and actually, only the first registration is counted in the
#' new registered users metric, since the system then automatically creates
#' user accounts at visit of other projects, and automatic account creations
#' are not counted.
#'
#' However, for data *before* CentralAuth, we have no way to know if two
#' accounts with the same name on different wikis belonged to the same user or
#' not. The metric provided for periods before CentralAuth does NOT deduplicate
#' accounts by name across projects, and therefore can be somehow overcounting
#' when used with project-families.
#' @section Granularity:
#' For "monthly" `granularity`, the `start_date` and `end_date` need to contain
#' the full month.
#'
#' For example, `start_date = "20191101"` and
#' `end_date = "20191231"` would only give the monthly total for 2019-11. To
#' include 2019-12, use `end_date = "20200101"`.
#'
#' Frustratingly, `start_date = "20191201"` and `end_date = "20191231"` does
#' **_not_** yield 2019-12 monthly total. Use `end_date = "20200101"` for that.
#' @inheritSection wx_query_api License
#' @return A tibble data frame with columns `date` and `new_registered_users`.
#' @seealso [wikitech:Analytics/AQS/Wikistats 2](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Wikistats_2)
#' @examples
#' wx_newly_registered_users(
#'   "mr.wikipedia", # Marathi
#'   granularity = "monthly",
#'   start_date = "20190401",
#'   end_date = "20191001"
#' )
#' @family {data & metrics on users}
#' @export
wx_newly_registered_users <- function(
  project,
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20191231"
) {
  project <- project[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity) %<-% wx_check_args(
    list("granularity" = granularity),
    args[c("granularity")]
  )
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  path <- paste(
    "/registered-users/new",
    project, granularity, start_date, end_date,
    sep = "/"
  )
  result <- wx_query_api(reqs_per_second = 25)(path)
  # Tidy up results:
  data_frame <- result$items[[1]]$results %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(date = as.Date(lubridate::ymd_hms(timestamp))) %>%
    dplyr::select(date, new_registered_users)
  return(data_frame)
}
