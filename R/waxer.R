#' @title waxer: Wikimedia Analytics Query Service (AQS) API
#' @description This is an R wrapper for the
#'   [Wikimedia Analytics Query Service (AQS)](https://www.mediawiki.org/wiki/Wikidata_query_service).
#'   The [REST API](https://wikimedia.org/api/rest_v1/) provides cacheable and
#'   straightforward access to Wikimedia content and data, in machine-readable
#'   formats. This particular wrapper is for the `/metrics` endpoint.
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
#' @section Metrics:
#' \describe{
#'   \item{[wx_active_editors]}{Number of active editors}
#' }
#' @aliases waxer
#' @docType package
#' @name waxer-package
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
NULL
