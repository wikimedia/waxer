.onLoad <- function(libname, pkgname) {
  msg <- "New in 0.9.5: In addition to \"bot\"/\"spider\" and \"user\" agent types,
there is now an \"automated\" agent type for traffic metrics.

The automated-traffic detection method changes the agent type field
of some pageviews from \"user\" to \"automated\". Therefore the number
of user pageviews decreased when the mechanism was deployed 2020-05-01.

See https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/BotDetection"
  message(msg)
}
