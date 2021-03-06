% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/content.R
\name{wx_page_edits}
\alias{wx_page_edits}
\title{Page edit counts}
\usage{
wx_page_edits(
  project,
  page_name,
  editor_type = c("all", "anonymous", "user", "bot", "bot group"),
  granularity = c("daily", "monthly"),
  start_date = "20191101",
  end_date = "20200101"
)
}
\arguments{
\item{project}{The name of any Wikimedia project formatted like
\verb{\{language code\}.\{project name\}}, for example en.wikipedia. You may pass
en.wikipedia.org and the .org will be stripped off. For projects like
Wikimedia Commons (without language codes), use commons.wikimedia. For
projects like www.mediawiki.org, you can use that full string, or just use
mediawiki or mediawiki.org.}

\item{page_name}{The title of any article in the specified project.
The function takes care of replacing spaces with underscores and
URI-encoding, so that non-URI-safe characters like \%, / or ? are accepted
-- e.g. "Are You the One?" becomes "Are_You_the_One\%3F". Internally this
is done with a non-exported \code{wx_encode_page_name} function. If you need to
get the pageviews for multiple pages, you're encouraged to provide all the
page names at once as this function has been optimized for that use-case.}

\item{editor_type}{If you want to filter by editor-type, use one of
"anonymous", "group-bot" (registered accounts belonging to the bot group),
"name-bot" (registered accounts not belonging to the bot group but having
bot-like names) or "user" (registered account not in bot group nor having
bot-like name). If you are interested in edits regardless of their editor
type, use "all-editor-types" (default).}

\item{granularity}{The time unit for the response data. As of today,
supported values are daily (default) and monthly.}

\item{start_date}{The date of the first day to include, in YYYYMMDD format.
Can also be a \code{Date} or a \code{POSIXt} object, which will be auto-formatted.}

\item{end_date}{The date of the last day to include, in YYYYMMDD format.
Can also be a \code{Date} or a \code{POSIXt} object, which will be auto-formatted.}
}
\value{
A tibble data frame with the following columns:
\describe{
\item{\code{project}}{project}
\item{\code{page_name}}{page name}
\item{\code{date}}{\code{Date}}
\item{\code{edits}}{Number of edits made to the page}
}
}
\description{
Number of edits for given page(s) and timespan. \strong{Note}: Due
to performance of the data-serving backend, this API endpoint limits the
queryable timespan to one year (whether in daily or monthly granularity).
}
\section{Granularity}{

For "monthly" \code{granularity}, the \code{start_date} and \code{end_date} need to contain
the full month.

For example, \code{start_date = "20191101"} and
\code{end_date = "20191231"} would only give the monthly total for 2019-11. To
include 2019-12, use \code{end_date = "20200101"}.

Frustratingly, \code{start_date = "20191201"} and \code{end_date = "20191231"} does
\strong{\emph{not}} yield 2019-12 monthly total. Use \code{end_date = "20200101"} for that.
}

\section{License}{

Data retrieved from the API endpoint is available under the
\href{https://creativecommons.org/publicdomain/zero/1.0/}{CC0 1.0 license}.
}

\examples{
\dontrun{
wx_page_edits(
  "en.wikipedia",
  c("Animal Crossing: New Horizons", "Animal Crossing"),
  granularity = "monthly",
  start_date = "20190101",
  end_date = "20191231"
)
}
}
\seealso{
Other {content data & metrics}: 
\code{\link{wx_edited_pages}()},
\code{\link{wx_mediareqs_file}()},
\code{\link{wx_mediareqs_referer}()},
\code{\link{wx_most_requested_files}()},
\code{\link{wx_new_pages}()},
\code{\link{wx_project_edits}()},
\code{\link{wx_top_edited_pages}()},
\code{\link{wx_total_pages}()}
}
\concept{{content data & metrics}}
