
<!-- README.md is generated from README.Rmd. Please edit that file -->

# natalia

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to support me (Natalia) with my various Data
Science tasks. In practice, this is just an eclectic collection of
personal functions that I keep on coming back to. In the future, some of
the functions might get migrated to their own niche packages where I’m
sure they’ll feel more at home!

<!--# TODO: add a hex sticker to left of this intro {hexSticker} -->

## Installation

You can install `natalia` from github with:

``` r
# install.packages("remotes")
# remotes::install_github("natalia-mlad/natalia")
```

## Examples

<!-- (maybe further broken down by usecases) -->

Here are some basic examples of how to use the functions found in this
package:

``` r
library(natalia)
```

-   ID ordinal variables from numeric ones

The following example is based on a cut-off point of 5, the max number
of levels allowed for the variable before it stops being considered
ordinal and becomes continuous.

``` r
isOrd(mtcars, n = 5)
#> [1] "cyl"  "vs"   "am"   "gear"
```

-   Polytomize item data

For instance, you could simplify Likert item data into three categories
for reporting or to use in further modeling.

``` r
responses <- data.frame(
  item1 = sample(1:7, 50, replace = TRUE),
  item2 = sample(1:7, 50, replace = TRUE),
  item3 = sample(1:7, 50, replace = TRUE)
)
head(responses)
#>   item1 item2 item3
#> 1     3     1     3
#> 2     5     6     6
#> 3     7     2     2
#> 4     5     2     2
#> 5     2     5     7
#> 6     1     6     7
```

``` r
polytomize_data(responses, thresholds = c(3,4)) %>%
  head()
#> [33mi[39m Turning data into 3 categories.
#>   item1 item2 item3
#> 1     1     1     1
#> 2     3     3     3
#> 3     3     1     1
#> 4     3     1     1
#> 5     1     3     3
#> 6     1     3     3
```

-   Scrape duckduckgo search results

This is useful for comparing top results and their sentiment across
search enginges.

``` r
duckduckgo(c("personal", "data", "privacy")) %>%
  head()
#> [1] "What is personal data? | ICO - Information Commissioner's Office\nico.org.uk/for-organisations/guide-to-data-protection/guide-to-the-general-data-protection-regulation-gdpr/what-is-personal-data/what-is-personal-data/\nThis means personal data has to be information that relates to an individual. That individual must be identified or identifiable either directly or indirectly from one or more identifiers or from factors specific to the individual. The UK GDPR covers the processing of personal data in two ways:"                                           
#> [2] "Data protection: The Data Protection Act - GOV.UK\nwww.gov.uk/data-protection\nThe Data Protection Act 2018 is the UK's implementation of the General Data Protection Regulation (GDPR). Everyone responsible for using personal data has to follow strict rules called..."                                                                                                                                                                                                                                                                                                   
#> [3] "What is personal data? | ICO - Information Commissioner's Office\nico.org.uk/for-organisations/guide-to-data-protection/guide-to-the-general-data-protection-regulation-gdpr/key-definitions/what-is-personal-data/\nPersonal data may also include special categories of personal data or criminal conviction and offences data. These are considered to be more sensitive and you may only process them in more limited circumstances. Pseudonymised data can help reduce privacy risks by making it more difficult to identify individuals, but it is still personal data."
#> [4] "What is data privacy? | Privacy definition | Cloudflare\nwww.cloudflare.com/learning/privacy/what-is-data-privacy/\nData privacy generally means the ability of a person to determine for themselves when, how, and to what extent personal information about them is shared with or communicated to others. This personal information can be one's name, location, contact information, or online or real-world behavior."                                                                                                                                                   
#> [5] "Privacy - data.gov.uk\nwww.data.gov.uk/privacy\nThe data controller for GDS is the Cabinet Office — a data controller determines how and why personal data is processed. For more information read the Cabinet Office's entry in the Data Protection Public ... Children's privacy protection. We understand the importance of protecting children's privacy online. Our service is not designed ..."                                                                                                                                                                         
#> [6] "What counts as personal data? - Which?\nwww.which.co.uk/consumer-rights/advice/what-counts-as-personal-data-a4T2s2Y2ffXd\nThe EU-wide rules in the Data Protection Act 2018 (GDPR) provides the legal definition of what counts as personal data in the UK. Personal data includes an identifier like: your name. an identification number, for example your National Insurance or passport number. your location data, for example your home address or mobile phone GPS data."
```

And so much more! (over 40 functions)
