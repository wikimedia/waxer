library(dplyr)
library(here)
library(jsonlite)
library(purrr)
library(readr)

url <- "https://www.mediawiki.org/w/api.php?action=sitematrix&format=json&smtype=language&formatversion=2"
json <- fromJSON(url, simplifyVector = FALSE)

wikis <- map_dfr(
  json$sitematrix[-1], # first element is $count
  function(language) {
    if ("localname" %in% names(language)) {
      lang <- language$localname
    } else {
      lang <- as.character(NA)
    }
    if ("code" %in% names(language)) {
      code <- language$code
    } else {
      code <- as.character(NA)
    }
    if ("dir" %in% names(language)) {
      direction <- language$dir
    } else {
      direction <- as.character(NA)
    }

    if (length(language$site) > 0) {
      sites <- map_dfr(language$site, as.data.frame, stringsAsFactors = FALSE)
      if (!"closed" %in% names(sites)) {
        sites$closed <- FALSE
      }
      sites$language <- lang
      sites$language_code <- code
      sites$direction <- direction
      output <- sites %>%
        select(language, language_code, project_code = code, url, direction, closed)
      return(output)
    } else {
      message(language, " does not have 'site'")
      return(NULL)
    }
  }
)

wikis %>%
  mutate(
    project_code = if_else(project_code == "wiki", "wikipedia", project_code),
    direction = if_else(direction == "ltr", "left-to-right", "right-to-left"),
    closed = if_else(is.na(closed), FALSE, closed),
    url = sub("https://", "", url, fixed = TRUE)
  ) %>%
  filter(!closed) %>%
  select(-closed) %>%
  arrange(language_code, project_code) %>%
  write_csv(here("inst", "extdata", "wikis.csv"))
