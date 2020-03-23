waxer: Wikimedia Analytics Query Service (AQS) API in R
================

  - [Installation](#installation)
  - [Usage](#usage)
  - [Additional Information](#additional-information)

This is an R wrapper for the [Wikimedia Analytics Query Service
(AQS)](https://www.mediawiki.org/wiki/Wikidata_query_service). The [REST
API](https://wikimedia.org/api/rest_v1/) provides cacheable and
straightforward access to Wikimedia content and data, in
machine-readable formats. This particular wrapper is for the `/metrics`
endpoint.

**Author:** Mikhail Popov (Wikimedia Foundation)<br/> **License:**
[MIT](http://opensource.org/licenses/MIT)<br/> **Status:** In early
development

## Installation

To install the development version:

``` r
# install.packages("remotes")
remotes::install_github("bearloga/waxer")
```

## Usage

``` r
library(waxer)

?waxer
```

## Additional Information

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/bearloga/WikidataQueryServiceR/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
