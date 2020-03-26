#' @title Mediarequest counts per referer
#' @description Daily and monthly aggregation of hits to media files, split by
#'   the referrer Referrer means either external, internal, unknown, or any
#'   Wikimedia wiki that the resource was requested from.
#' @param referer The place that the request was made from. It can be any
#'   Wikimedia project (e.g. de.wikipedia), "all", "internal", "external",
#'   "search-engine", "unknown" or "none".
#' @param media_type The media type that each file belongs to. It can be
#'   image, audio, video, document, or other. Default is "all" media types.
#'   See Media section below.
#' @inheritParams wx_top_edited_pages
#' @inheritParams wx_project_views
#' @section Media:
#' File types are obtained by parsing the file extension, and then classified
#' according to the following table:
#'
#' | Extensions | Media type |
#' | :--------- | :--------- |
#' | svg, png, tiff, tiff, jpeg, jpeg, gif, xcf, webp, bmp | image |
#' | mp3, ogg, oga, flac, wav, midi, midi | audio |
#' | webm, ogv | video |
#' | pdf, djvu, srt, txt | document |
#' | (all other extensions) | other |
#' @section Limitations:
#' - The ability of splitting and filtering by referrer is limited to data
#'   from May 2019 onward. Before that, referrer is only split in internal,
#'   external, and unknown.
#' - The beginning of mediarequest data is the 1st of January 2015.
#' - The ability of splitting and filtering by agent type (user, spider) is
#'   limited to data from May 2019 onward.
#' - About 0.7% of mediarequests are prefetches coming from Media Viewer
#'   (more details in [wikitech:Analytics/AQS/Media_metrics](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Media_metrics))
#' @inheritSection wx_query_api License
#' @examples
#' wx_mediareqs_referer("de.wikipedia", granularity = "monthly")
#' @seealso
#' - [wikitech:Analytics/AQS/Mediarequests](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Mediarequests)
#' @export
wx_mediareqs_referer <- function(
  referer,
  media_type = c("all", "image", "video", "audio", "document", "other"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly"),
  start_date = "20191201",
  end_date = "20200101"
) {
  referer <- referer[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, media_type, agent_type) %<-% wx_check_args(
    list("granularity" = granularity, "media_type" = media_type, "agent_type" = agent_type),
    args[c("granularity", "media_type", "agent_type")]
  )
  c(media_type, agent_type) %<-% wx_types(media = media_type, agent = agent_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  referer <- ifelse(referer == "all", "all-referers", referer)
  path <- glue::glue("/mediarequests/aggregate/{referer}/{media_type}/{agent_type}/{granularity}/{start_date}/{end_date}")
  result <- wx_query_api(reqs_per_second = 100)(path)
  data_frame <- result$items %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    dplyr::mutate(date = as.Date(timestamp, "%Y%m%d00")) %>%
    dplyr::select(date, requests)
  return(data_frame)
}

#' @title Mediarequest counts for a media file
#' @description Daily and monthly counts of mediarequests for each media file
#'   stored in the wiki servers (as long as the count is higher than one).
#' @inheritParams wx_top_edited_pages
#' @inheritParams wx_project_views
#' @inheritParams wx_mediareqs_referer
#' @param file_path One or more paths to the file on `upload.wikimedia.org`,
#'   which is the file storage for all media in any wiki.
#' @inheritSection wx_mediareqs_referer Media
#' @inheritSection wx_mediareqs_referer Limitations
#' @inheritSection wx_query_api License
#' @section Example usage:
#' The example below retrieves the request counts for
#' [File:Hadley-wickham2016-02-04.jpg](https://commons.wikimedia.org/wiki/File:Hadley-wickham2016-02-04.jpg)
#' which is used on the English Wikipedia article
#' [*Hadley Wickham*](https://en.wikipedia.org/wiki/Hadley_Wickham)
#' as the main image in the infobox.
#' @examples
#' wx_mediareqs_file(
#'   "/wikipedia/commons/f/fa/Hadley-wickham2016-02-04.jpg", "en.wikipedia",
#'   agent_type = "user", granularity = "monthly",
#'   start_date = "20200101", end_date = "20200301"
#' )
#' @export
wx_mediareqs_file <- function(
  file_path, referer,
  media_type = c("all", "image", "video", "audio", "document", "other"),
  agent_type = c("all", "user", "bot"),
  granularity = c("daily", "monthly"),
  start_date = "20191201",
  end_date = "20200101"
) {
  referer <- referer[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, media_type, agent_type) %<-% wx_check_args(
    list("granularity" = granularity, "media_type" = media_type, "agent_type" = agent_type),
    args[c("granularity", "media_type", "agent_type")]
  )
  c(media_type, agent_type) %<-% wx_types(media = media_type, agent = agent_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  referer <- ifelse(referer == "all", "all-referers", referer)
  file_paths <- wx_encode_page_name(file_path)
  results <- purrr::map_dfr(file_paths, function(file_path) {
    path <- glue::glue("/mediarequests/per-file/{referer}/{agent_type}/{file_path}/{granularity}/{start_date}/{end_date}")
    result <- wx_query_api(reqs_per_second = 100)(path)
    data_frame <- result$items %>%
      purrr::map_dfr(dplyr::as_tibble) %>%
      dplyr::mutate(date = as.Date(timestamp, "%Y%m%d00")) %>%
      dplyr::select(date, requests)
    return(data_frame)
  }, .id = "file_name")
  return(results)
}

#' @title Most requested files for a referer
#' @description The most requested media files per referer and per media type.
#' @inheritParams wx_top_edited_pages
#' @inheritParams wx_project_views
#' @inheritParams wx_mediareqs_referer
#' @inheritSection wx_mediareqs_referer Media
#' @inheritSection wx_mediareqs_referer Limitations
#' @inheritSection wx_query_api License
#' @examples
#' wx_most_requested_files("en.wikipedia", media_type = "video", granularity = "monthly")
#' @export
wx_most_requested_files <- function(
  referer,
  media_type = c("all", "image", "video", "audio", "document", "other"),
  granularity = c("daily", "monthly"),
  start_date = "20191231",
  end_date = "20200101"
) {
  referer <- referer[1] # force 1 project per call
  # Validate "one of" arguments:
  args <- formals()
  c(granularity, media_type) %<-% wx_check_args(
    list("granularity" = granularity, "media_type" = media_type),
    args[c("granularity", "media_type")]
  )
  c(media_type) %<-% wx_types(media = media_type)
  c(start_date, end_date) %<-% wx_format_dates(start_date, end_date)
  c(start_date, end_date) %<-% as.Date(c(start_date, end_date), "%Y%m%d")
  dates <- seq(start_date, end_date, by = "day")
  referer <- ifelse(referer == "all", "all-referers", referer)
  query <- wx_query_api(reqs_per_second = 100)
  if (granularity == "daily") {
    results <- purrr::map_dfr(dates, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      path <- glue::glue("/mediarequests/top/{referer}/{media_type}/{year}/{month}/{day}")
      result <- query(path)
      data_frame <- result$items[[1]]$files %>%
        purrr::map_dfr(dplyr::as_tibble) %>%
        dplyr::mutate(date = date) %>%
        dplyr::select(date, file_path, requests, rank)
      return(data_frame)
    })
    return(results)
  } else {
    months <- unique(lubridate::floor_date(dates, unit = "month"))
    results <- purrr::map_dfr(months, function(date) {
      c(year, month, day) %<-% wx_extract_yyyymmdd(date)
      day <- "all-days"
      path <- glue::glue("/mediarequests/top/{referer}/{media_type}/{year}/{month}/{day}")
      tryCatch(
        {
          result <- query(path)
          data_frame <- result$items[[1]]$files %>%
            purrr::map_dfr(dplyr::as_tibble) %>%
            dplyr::mutate(date = date) %>%
            dplyr::select(date, file_path, requests, rank)
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
