##
# Key steps that accelerate your R development workflow (details on how to do all this follow):
# - Make usethis available in interactive R sessions.
# - Provide usethis with info to use in all new R packages you create.
# - Use the “sitrep” functions to get a health check or gather info when you’re going to ask for help.
# - Configure your Git user.name and user.email.
# - If you use RStudio, make sure RStudio can find your Git executable. If you use GitHub, make sure you can pull/push from your local computer to GitHub.com, in general and from RStudio.
# - Get a personal access token from GitHub.com and make it available in R sessions.
# - Prepare your system to build R packages from source.
##
# Configuration - Configure the behaviour of R or RStudio, globally as a user or for a specific project.
#  use_blank_slate() #= Don't save/load user workspace between sessions
# Helpers to make useful changes to .Rprofile:
#  use_conflicted() use_reprex() use_usethis() use_devtools() use_partial_warnings()
# Open configuration files:
#  edit_r_profile() edit_r_environ() edit_r_buildignore() edit_r_makevars()
#  edit_rstudio_prefs() edit_git_config() edit_git_ignore()
###

# Troubleshooting & Health Checks -----------------------------------------
has_devel(); proj_sitrep()#; git_sitrep()
devtools::document(); devtools::check()

# 1. Package Setup -------------------------------------------------
#library(histry)
#library(tidyverse)
library(fs)
library(usethis)
library(attachment)
#create the pkg skeleton:
##
# path <- fs::path_home("OneDrive/pkgs/natalia")
# usethis::create_package(path)
# dir_create("dev")
# file_create("dev/dev_history.R")
# file_show("dev/dev_history.R")
# usethis::use_build_ignore("dev")
##
#histry() #saveHistry()
#configure the necessary options:
use_roxygen_md()
use_package_doc()
use_readme_rmd()
#build_readme()
##
edit_r_profile(scope = c("project"))
usethis::use_devtools()
usethis::use_pipe()
devtools::document()
##
use_testthat()
use_test("placeholder")
devtools::document()
devtools::check()
use_git()
use_github(private = TRUE)
git_vaccinate()
##
#citEntry() #create pkg citation
#https://rdrr.io/r/utils/citEntry.html
##
#Run attachment::att_amend_desc() each time before devtools::check(),
#this will save you some warnings and errors !
attachment::att_amend_desc()
#att_amend_desc(path = dummypackage, inside_rmd = TRUE)
##
#use_git_config(core.editor = "nano")
# TODO:
#use_package("MASS", "Suggests")
#Adding 'MASS' to Suggests field in DESCRIPTION
#Use `requireNamespace("MASS", quietly = TRUE)` to test if package is installed
#Then directly refer to functons like `MASS::fun()` (replacing `fun()`).
##
"natalia: A package for ..."
"Description: {I will add a description later...}"
##
#use_readme_md() #Writing 'README.md'
#build_readme()
##
use_proprietary_license("Natalia Mladentseva")
##
# TODO: use_git()
##
testthat::test_path()
##
use_tidy_github_actions()
use_tidy_description()
use_tidy_eval()
use_tidy_support()
use_tidy_issue_template()
use_tidy_coc()
use_tidy_github()
use_tidy_style(strict = TRUE)
use_tidy_release_test_env()

# gives you instructions and all
usethis::use_devtools()

# Writing Functions: ------------------------------------------------------
fbind <- function(a, b) {
  factor(c(as.character(a), as.character(b)))
}
use_r("fbind")
use_test("my-test")
#Adding 'testthat' to Suggests field in DESCRIPTION
#Setting Config/testthat/edition field in DESCRIPTION to '3'
#Creating 'tests/testthat/'
#Writing 'tests/testthat.R'
#Writing 'tests/testthat/test-my-test.R'
#Edit 'tests/testthat/test-my-test.R'

example_x <- 1
example_y <- 2
use_data(example_x, example_y) #If the DESCRIPTION contains LazyData: true, then datasets will be lazily loaded. This means that they won’t occupy any memory until you use them.

#Adding 'R' to Depends field in DESCRIPTION
#Creating 'data/'
#Setting LazyData to 'true' in 'DESCRIPTION'
#Saving 'x', 'y' to 'data/x.rda', 'data/y.rda'
#Document your data (see 'https://r-pkgs.org/data.html')

# Often, the data you include in data/ is a cleaned up version of raw data you’ve gathered from elsewhere. I highly recommend taking the time to include the code used to do this in the source version of your package. This will make it easy for you to update or reproduce your version of the data. I suggest that you put this code in data-raw/. You don’t need it in the bundled version of your package, so also add it to .Rbuildignore. Do all this in one step with:
#
# usethis::use_data_raw()
# You can see this approach in practice in some of my recent data packages. I’ve been creating these as packages because the data will rarely change, and because multiple packages can then use them for examples:
#
# babynames
# fueleconomy
# nasaweather
# nycflights13
# usdanutrients


# Adding Data: ------------------------------------------------------------
#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"diamonds"

# There are two additional tags that are important for documenting datasets:
#
# @format gives an overview of the dataset. For data frames, you should include a definition list that describes each variable. It’s usually a good idea to describe variables’ units here.
#
# @source provides details of where you got the data, often a \url{}.
#
# Never @export a data set.


# Documentation: ------------------------------------------------------------
#Use Ctrl + . in RStudio and start typing “DESCRIPTION” to activate a helper that makes it easy to open a file for editing. In addition to a filename, your hint can be a function name. This is very handy once a package has lots of functions and files below R/.
##
# Functions:
#' @param
#' @param
#'
#' @return
#' @export
#' @examples
#'
document()
##
# edits/creates:
# -NAMESPACE
# -man/functionName.Rd
##


# Simple Health Checks: ---------------------------------------------------
check()
install()
library(yourPkgName)
#Example of how to simulate installing and loading a package, during interactive development:
pkgload::load_all()
#Also:
devtools::load_all() #But pkgload is preferred

# tests -------------------------------------------------------------------
use_testthat()
use_test("fbind")
test_that("fbind() binds factor (or character)", {
  x <- c("a", "b")
  x_fact <- factor(x)
  y <- c("c", "d")
  z <- factor(c("a", "b", "c", "d"))

  expect_identical(fbind(x, y), z)
  expect_identical(fbind(x_fact, y), z)
})
test()

## Testing:
load_all()
check()
##
#patrick
##
tinytest::setup_tinytest(".")
library(tinytest)
library(checkmate)
using("checkmate")
##
rcmdcheck::rcmdcheck()
#https://www.tidyverse.org/blog/2018/10/devtools-2-0-0/#conscious-uncoupling
# Testing single files:
# devtools now includes functions (test_file() and test_coverage_file()) to improve development of a single file. Rather than running all tests, or manually supplying a filter argument to restrict the tests test_file() automatically runs the corresponding tests for a given source file. These functions make the feedback loop when developing new features quicker as you only run the relevant tests for the file you are editing.
# This requires you use a standard naming convention for your tests, e.g. if you have a source file R/featureA.R the corresponding test file would be tests/testthat/test-featureR.R.
# The tests file to run is automatically detected from the open file in RStudio (if available), so you can call test_file() with either the source file or the test file open. A corresponding test_coverage_file() function shows the test code coverage for a single source file.
# There is also a test_coverage() function to report test coverage for your whole package.
# These functions have RStudio addins which allows you to bind them to shortcut keys.
revdep_check()



# style R code ------------------------------------------------------------
# 7.3 Code style
# We recommend following the tidyverse style guide (https://style.tidyverse.org), which goes into much more detail than we can here. Its format also allows it to be a more dynamic document than this book.

# Although the style guide explains the “what” and the “why”, another important decision is how to enforce a specific code style. For this we recommend the styler package (https://styler.r-lib.org); its default behaviour enforces the tidyverse style guide. There are many ways to apply styler to your code, depending on the context:

styler::style_pkg() #restyles an entire R package.
styler::style_dir() #restyles all files in a directory.
usethis::use_tidy_style() #is wrapper that applies one of the above functions depending on whether the current project is an R package or not.
styler::style_file() #restyles a single file.
styler::style_text() #restyles a character vector.
##
# When styler is installed, the RStudio Addins menu will offer several additional ways to style code:
#
# the active selection
# the active file
# the active package
# The use of Git or another version control system is optional, but a recommended practice in the long-term. We explain its importance in 18. For example, it’s nerve-wracking and somewhat dangerous to apply a function like styler::style_pkg() without some way to see exactly what changed and to accept/reject such changes in a granular way.
#
# The styler package can also be integrated with various platforms for hosting source code and doing continuous integration. For example, the tidyverse packages use a GitHub Action that restyles a package when triggered by a special comment (/style) on a pull request. This allows maintainers to focus on reviewing the substance of the pull request, without having to nitpick small issues of whitespace or indentation1516.
##
# spellcheck for the documentation


# extra -------------------------------------------------------------------

usethis::use_make()
use_logo()
use_article()
use_vignette()
use_addin()


# pkgdown -----------------------------------------------------------------
# Run once to configure package to use pkgdown
use_pkgdown()
# Run to build the website
pkgdown::build_site()

# Extract scripts dependencies and generate your Description file ---------

# bookdown Imports are in Rmds
# imports <- c("bookdown", attachment::att_from_rmds("."))
# attachment::att_to_desc_from_is(path.d = "DESCRIPTION",
#                                 imports = imports, suggests = NULL)
att_from_description() %>% find_remotes()
att_from_namespace()
att_from_rscripts()
# Vignettes?
att_from_rmds()
# Create a file for package installation:
create_dependencies_file()
