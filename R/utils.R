wx_encode_page_name <- function(x) {
  encoded <- magrittr::set_names(urltools::url_encode(gsub(" ", "_", x, fixed = TRUE)), x)
  return(encoded)
}
wx_decode_page_name <- function(x) {
  decoded <- gsub("_", " ", x, fixed = TRUE)
  return(decoded)
}

wx_types <- function(...) {
  args <- list(...)
  if ("editor" %in% names(args)) {
    args$editor <- wx_editor_type(args$editor)
  }
  if ("page" %in% names(args)) {
    args$page <- wx_page_type(args$page)
  }
  if ("activity" %in% names(args)) {
    args$activity <- wx_activity_level(args$activity)
  }
  if ("access" %in% names(args)) {
    args$access <- wx_access_method(args$access)
  }
  if ("agent" %in% names(args)) {
    args$agent <- wx_agent_type(args$agent)
  }
  if ("media" %in% names(args)) {
    args$media <- wx_media_type(args$media)
  }
  return(args)
}
wx_agent_type <- function(x) {
  agent_types <- c(
    "all" = "all-agents",
    "user" = "user",
    "bot" = "spider",
    "spider" = "spider",
    "automated" = "automated"
  )
  return(agent_types[x])
}
wx_access_method <- function(x) {
  access_methods <- c(
    "all" = "all-access", "desktop" = "desktop",
    "mobile app" = "mobile-app", "mobile web" = "mobile-web"
  )
  return(access_methods[x])
}
wx_editor_type <- function(x) {
  editor_types <- c(
    "all" = "all-editor-types", "anonymous" = "anonymous",
    "user" = "user", "bot" = "name-bot", "bot group" = "group-bot"
  )
  return(editor_types[x])
}
wx_page_type <- function(x) {
  page_types <- c("all" = "all-page-types", "content" = "content", "non-content" = "non-content")
  return(page_types[x])
}
wx_activity_level <- function(x) {
  activity_levels <- c(
    "all" = "all-activity-levels",
    "1-4" = "1..4-edits", "5-24" = "5..24-edits",
    "25-99" = "25..99-edits", "100+" = "100..-edits"
  )
  return(activity_levels[x])
}
wx_media_type <- function(x) {
  media_types <- c(
    "all" = "all-media-types", "image" = "image", "video" = "video",
    "audio" = "audio", "document" = "document", "other" = "other"
  )
  return(media_types[x])
}

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
wx_format_dates <- function(start_date, end_date) {
  start_date <- wx_format_date(start_date)
  end_date <- wx_format_date(end_date)
  assertthat::assert_that(
    end_date >= start_date,
    msg = "[User error] end_date must be same as or later than start_date"
  )
  return(list(start_date, end_date))
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

#' @title Bucket items
#' @description Split a vector of items into buckets not exceeding some maximum
#'   bucket size. Useful for making fewer MediaWiki API calls when an endpoint
#'   accepts multiple page titles per call (for example).
#' @param items A vector of items
#' @param bucket_size Maximum number of items a bucket may have
#' @return A list with each element containing no more than `bucket_size` number
#'   of items. Flattening the returned list would yield the original `items`
#'   vector.
#' @export
wx_bucket_items <- function(items, bucket_size) {
  n_items <- length(items)
  if (n_items > bucket_size) {
    n_segments <- floor(n_items / bucket_size)
    n_remainder <- n_items %% bucket_size
    if (n_remainder > 0) {
      remainder_pages <- items[1:n_remainder]
      buckets <- unname(split(items[-(1:n_remainder)], 1:n_segments))
      buckets <- c(buckets, list(remainder_pages))
    } else {
      buckets <- unname(split(items, 1:n_segments))
    }
  } else {
    buckets <- list(items)
  }
  return(buckets)
}

wx_get_redirects <- function(project, page_names) {
  # Break up the list of pages in blocks of 50 or fewer,
  # which is the maximum limit for 1 call to redirect API:
  segments <- wx_bucket_items(page_names, 50)
  mw_query <- wx_mediawiki_api(project)
  purrr::map_dfr(segments, function(segment) {
    titles <- paste0(segment, collapse = "|")
    query <- glue::glue("action=query&format=json&prop=redirects&meta=&titles={titles}&rdnamespace=0&rdlimit=max")
    result <- mw_query(query)
    purrr::map_dfr(result$query$pages, function(page) {
      if (!"redirects" %in% names(page)) return(NULL)
      df <- purrr::map_dfr(page$redirects, as.data.frame, stringsAsFactors = FALSE, col.names = c("page_id", "namespace", "redirect_title"))
      df$page_title = page$title # unused: page_id = page$pageid
      return(df[, c("page_title", "redirect_title")])
    })
  })
}
