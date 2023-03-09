
<!-- README.md is generated from README.Rmd. Please edit that file -->

# natalia

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to support me with my various Data Science
tasks.

In practice, this is just an eclectic collection of personal functions
that I keep on coming back to. In the future, some of the functions
might get migrated to their own niche packages where I’m sure they’ll
feel more at home!

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
#> 1     6     3     6
#> 2     4     1     1
#> 3     3     7     2
#> 4     5     5     6
#> 5     1     4     1
#> 6     6     3     5
```

``` r
polytomize_data(responses, thresholds = c(3,4)) %>%
  head()
#> [33mi[39m Turning data into 3 categories.
#>   item1 item2 item3
#> 1     3     1     3
#> 2     2     1     1
#> 3     1     3     1
#> 4     3     3     3
#> 5     1     2     1
#> 6     3     1     3
```

-   Scrape duckduckgo search results

This is useful for comparing top results and their sentiments across
queries or search enginges.

``` r
duckduckgo("LGBT") %>% head()
#> [1] "LGBT - Wikipedia\nen.wikipedia.org/wiki/LGBT\nLGBT is an initialism that stands for lesbian, gay, bisexual, and transgender.In use since the 1990s, the initialism, as well as some of its common variants, functions as an umbrella term for sexuality and gender identity.. The LGBT term is an adaptation of the initialism LGB, which began to replace the term gay (or gay and lesbian) in reference to the broader LGBT community beginning in the ..."                           
#> [2] "LGBT Foundation - Home\nlgbt.foundation\nLGBT Foundation is a national charity delivering advice, support and information services to lesbian, gay, bisexual and trans (LGBT) communities. Equality Wins > Find out more about our campaign to ensure all LGBT people feel welcome, heard and represented. Get support >"                                                                                                                                                               
#> [3] "About LGBTQIA+ and mental health - Mind\nwww.mind.org.uk/information-support/tips-for-everyday-living/lgbtqia-mental-health/about-lgbtqia-mental-health/\nBut those of us who identify as LGBTQIA+ are more likely to develop problems like: low self-esteem depression anxiety, including social anxiety eating problems misusing drugs and alcohol self-harm suicidal feelings other mental health problems. Being LGBTQIA+ does not cause these problems."                           
#> [4] "We know what LGBT means but here's what LGBTQQIAAP stands for\nwww.bbc.co.uk/news/newsbeat-33278165\nWe know what LGBT stands for but there are many other terms people now identity with, giving us the acronym LGBTQQIAAP. The 10 terms cover the different ways people define their gender and..."                                                                                                                                                                                   
#> [5] "LGBTQ+ facts and figures | Stonewall\nwww.stonewall.org.uk/lgbtq-facts-and-figures\nAlmost one in five LGBT people (18 per cent) have experienced homelessness at some point in their lives. Half of black, Asian and minority ethnic LGBT people (51 per cent) have experienced discrimination or poor treatment from others in their local LGBT community because of their ethnicity."                                                                                                
#> [6] "What Does LGBT Mean? Know the Basics. | Youth Engaged 4 Change\nengage.youth.gov/resources/what-does-lgbt-mean-know-basics\nThe term \" LGBT \" technically stands for lesbian, gay, bisexual, and transgender. It includes both sexual orientation (LGB) and gender identity (T). But, it's sometimes used as an umbrella term for anyone who does not identify as straight (heterosexual) or cisgender, so it's important to know other sexual and gender identities the term covers."

duckduckgo(c("LGBT", "religion")) %>% head()
#> [1] "Religion and LGBT people - Wikipedia\nen.wikipedia.org/wiki/Religion_and_LGBT_people\nReligious views of LGBT people. According to a 2006 Australian survey, LGBT Australians, compared to the general Australian population, were much more likely to have no religious affiliation, much less likely to be affiliated with a Christian denomination, and more likely to be affiliated with a non-Christian religion.The distribution of religions that LGBT Australians were raised in, however ..."
#> [2] "What different religions say about the LGBT+ community\nnewrationalist.com/what-different-religions-say-about-the-lgbt-community/\nLGBT continues to be a contentious matter for several major religions of the world. A few religions have stringent preaching against homosexual and bisexual activities. Some religions are relatively less stringent. Very few religions have warmly welcomed the LGBT community around the world."                                                               
#> [3] "Being LGBTQ+ and Religious | LGBT HERO - the national health and ...\nwww.lgbthero.org.uk/being-lgbtq-and-religious\nThe myth that all people within the community have to be nonreligious or atheist can be harmful to other LGBTQ+ people who connect very deeply with their religious identity. It is unfortunately not uncommon for religious spaces to shun or reject people after coming out, sometimes forcing LGBTQ+ people out of the space entirely."                                       
#> [4] "LGBT-affirming religious groups - Wikipedia\nen.wikipedia.org/wiki/LGBT-affirming_religious_groups\nLesbian, gay, bisexual, and transgender ( LGBT) affirming religious groups, otherwise referred to as gay-affirming religious groups, are religious groups that welcome LGBT people as their members, do not consider homosexuality as a sin or negative, and affirm LGBT rights and relationships. They include entire religious denominations, as well as ..."                                   
#> [5] "LGBT and Religion in the UK | Changing Attitude\nchangingattitude.org.uk/lgbt-and-religion-in-the-uk/\nThe Church of England has a more mixed position on LGBT rights. The Church currently allows gay men and women, and trans men and women to serve in the clergy. Gay priests are allowed to enter into civil partnerships, but they are not allowed to have religious marriage ceremonies."                                                                                                      
#> [6] "The LGBT Experience With Spiritual, Religious, And Existential Issues\nwww.anxiety.org/lgbt-religion-faith-anxiety\nReligion is a Valuable Method for Coping with Stressors Religion's role in coping with the stressors LGBT people face is a complex one. For their heterosexual, cisgender counterparts, religious and spiritual beliefs and practices can provide strong support during difficult times."
```

And so much more! (over 40 functions)
