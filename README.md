Wikimedia Analytics Query Service (AQS) API in R
================

{waxer} is an R wrapper for the [Wikimedia Analytics Query Service
(AQS)](https://wikitech.wikimedia.org/wiki/Analytics/Systems/AQS). This
particular wrapper is for the `/metrics` endpoint of the [REST
API](https://wikimedia.org/api/rest_v1/) which provides data and metrics
around traffic, users, and content on Wikimedia sites.

**Author:** Mikhail Popov (Wikimedia Foundation)<br/> **License:**
[MIT](http://opensource.org/licenses/MIT)<br/> **Status:** Active

## Installation

To install the development version:

``` r
# install.packages("remotes")
remotes::install_github("wikimedia/waxer@main")
```

To update:

``` r
remotes::update_packages(packages = "waxer")
```

## Usage

``` r
library(waxer)
?waxer
```

## Additional Information

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

**Why “waxer”?** Well, I was looking at the initialism “WAQSR”
(Wikimedia Analytics Query Service in R) and just really not liking how
awkward it looked.
