#' @title waxer: Wikimedia Analytics Query Service (AQS) API
#' @description This is an R wrapper for the
#'   [Wikimedia Analytics Query Service (AQS)](https://www.mediawiki.org/wiki/Wikidata_query_service).
#'   The [REST API](https://wikimedia.org/api/rest_v1/) provides cacheable and
#'   straightforward access to Wikimedia content and data, in machine-readable
#'   formats. This particular wrapper is for the `/metrics` endpoint.
#' @section Definitions:
#' ## Editor type
#' The `editor_type` parameter in functions allows you to filter the type of
#' editors having performed the related action. Can be `anonymous` for users
#' not logged in,` bot group` for logged in users that are part of the bot
#' group, `bot` for logged in users whose name contains `bot` (high probability
#' of being a bot, even if counter examples exists), and` user` for logged-in
#' users not in `bot group` nor `bot` sets. Finally, you can use `all` to not
#' filter by editor type.
#' ## Page type
#' The `page_type` parameter in functions allows you to filter for the type of
#' page over which the action is performed. Can be `content` for pages
#' belonging in content namespaces, also known as "main" namespaces. This page
#' type is also referred to as articles, and for most wikis includes pages in
#' namespace 0 only. Can also be `non-content` for pages in namespaces not
#' considered content (talk pages, user pages etc). Finally, you can use `all`
#' to not filter by page type.
#' @section Wikimedia projects:
#' The package comes bundled with a list of *active* Wikibooks, Wikinews,
#' Wikipedia, Wikiquote, Wikisource, Wikiversity, Wikivoyage, and Wiktionary
#' wikis.
#'
#' ```
#' wikis <- read.csv(system.file("extdata/wikis.csv", package = "waxer"))
#' ```
#'
#' The columns are:
#' \describe{
#'   \item{`language`}{e.g. "English", "Arabic"}
#'   \item{`language_code`}{e.g. "en", "ar"}
#'   \item{`project_code`}{e.g. "wikipedia", "wikibooks"}
#'   \item{`url`}{e.g. "en.wikipedia.org", "ar.wikibooks.org"}
#'   \item{`direction`}{e.g. "left-to-right", "right-to-left"}
#' }
#'
#' This dataset was generated from the
#' [Wikimedia site matrix](https://www.mediawiki.org/wiki/Special:SiteMatrix).
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
"_PACKAGE"
