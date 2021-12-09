#####
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


# latest r cmd check results: ####
# -- R CMD check results ------------------ natalia 0.0.0.1 ----
#   Duration: 1m 5.3s
#
# > checking top-level files ... NOTE
# Non-standard files/directories found at top level:
#   'README.nb.html' 'dev'
#
# > checking R code for possible problems ... NOTE
# bootReg: no visible binding for global variable 'SumOpportunitiesToPay'
# change.range: no visible binding for global variable 'training'
# change.range: no visible binding for global variable 'col_names'
# change.range: no visible binding for global variable 'max.new'
# change.range: no visible binding for global variable 'min.new'
# change.range: no visible binding for global variable 'x'
# clean_journal_tables: no visible binding for global variable 'V1'
# clean_journal_tables: no visible binding for global variable 'SJR'
# clean_journal_tables: no visible binding for global variable 'Cites /
#     Doc. (2years)'
# clean_journal_tables: no visible binding for global variable 'Ref. /
#     Doc.'
# corr.boot: no visible binding for global variable 'data10'
# corr_simple: no visible binding for global variable 'df'
# corr_simple: no visible binding for global variable 'Freq'
# df_to_df: no visible binding for global variable 'link'
#
# flat_cor_mat: no visible binding for global variable 'column'
# flat_cor_mat: no visible binding for global variable 'cor'
# flat_cor_mat: no visible binding for global variable 'p'
#
# identify_redundant_ids: no visible binding for global variable 'ids'
# identify_redundant_ids: no visible binding for global variable
# 'factors'
# identify_redundant_ids: no visible binding for global variable 'dist'
# identify_redundant_ids: no visible binding for global variable
# 'is_nested'
# isOrd: no visible binding for global variable '.'
# make_article: no visible binding for global variable 'article'
# page_to_df_recursive: no visible binding for global variable '.'
# polytomize_data: no visible binding for global variable 'exLong'

###
# my_env: warning in ls(all = T): partial argument match of 'all' to
# 'all.names'
#
# fit_fun: no visible binding for global variable 'nonlin_form'
# fit_fun: no visible global function definition for 'analysis'
# fit_fun: no visible global function definition for 'tidy'
# fold_incr: no visible global function definition for 'analysis'
# get_poly: no visible binding for global variable 'is_continuous'
# get_poly: no visible global function definition for 'N_distinct'
# get_poly: no visible global function definition for 'get_interactions'
# get_poly: no visible global function definition for 'model_matrix'
# remap.distance: no visible global function definition for 'missingMsg'
# simulate_data_continuous: no visible global function definition for
# 'rhcauchy'
# simulate_data_discrete: no visible global function definition for
# 'rhcauchy'

###
# Undefined global functions or variables:
# . Cites / Doc. (2years) Freq N_distinct Ref. / Doc. SJR
# SumOpportunitiesToPay V1 abort analysis article check_file_name
# col_names column cor data10 df dist exLong factors get_active_r_file
# get_interactions ids is_continuous is_nested link max.new min.new
# missingMsg model_matrix nonlin_form p rhcauchy slug tidy training
# where x
#
# Consider adding
# importFrom("base", "/")
# importFrom("stats", "cor", "df", "dist")
# to your NAMESPACE file.
#
# 0 errors v | 0 warnings v | 2 notes x
#
# R CMD check succeeded


# Import From -------------------------------------------------------------
library(origin)

automagic::get_dependent_packages("R")

originize_file("R/ds-communicate.R")
originize_dir("R", pkgs = "CodeDepends")
originize_dir("R", pkgs = "tidyr")

# installed.packages()[,1]
originize_file("R/flextable.R", pkgs = "flextable")
#

# use_import_from("fs", "dir_info")
use_import_from("stringr", c("str_detect", "str_remove_all", "str_replace"))
use_import_from("archive", "archive")
use_import_from("utils", c("adist", "ls.str", "sessionInfo", "write.table"))

# sinew::makeImport()
(desc = sort(att_from_description()))
(rscripts = sort(att_from_rscripts()))
setdiff(desc, rscripts)
setdiff(rscripts, desc)
##

pkgs <- unique(c(desc, rscripts, .packages())) %>%
  str_subset(c("stringr|dplyr|tidyr|purrr|tibble|fs"), negate = T)

originize_dir("R", pkgs = pkgs)

originize_dir("R", pkgs = "stats", add_base_packages = T)
originize_dir("R", pkgs = "methods", add_base_packages = T)


att_from_description() %>% find_remotes()

# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
# https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/globalVariables
# https://stackoverflow.com/questions/40251801/how-to-use-utilsglobalvariables
origin::get_exported_functions("CodeDepends")


####
# Find All User Defined functions in the Project
origin::get_local_functions()
# path_home("OneDrive/PhD Psychology/01 - R Project") %>% dir_ls(type = "directory")
# path_home("OneDrive/PhD Psychology/01 - R Project") %>% dir_ls(glob = "*.Rproj", recurse = 1)
path_home("OneDrive/PhD Psychology/01 - R Project/rbook-master/") %>%
  origin::get_local_functions(path = .)
##
# Get Packages from the DESCRIPTION file
origin::get_pkgs_from_description()


path_home("OneDrive/PhD Psychology/03 - Oakam Work") %>%
  origin::get_local_functions(path = .)

path_home("OneDrive/PhD Psychology/01 - Investigation - Developing Creditworthiness Measure")  %>%
  origin::get_local_functions(path = .)


# {dupree} ####
path <- path_home("OneDrive/PhD Psychology/01 - Investigation - Developing Creditworthiness Measure/04 - Study 3 - Validating Measure/")
x <- dupree::dupree_dir(path = path, recursive = F)
View(as_tibble(x))
# x = x[[1]]

# files <- dir(path, pattern = ".*.R$", recursive = F, full.names = T)
# dupree(files, min_block_size)

y <- as_tibble(x) %>%
  mutate(
    a_file = path_file(file_a),
    b_file = path_file(file_b),
    file_a_contents = map(file_a, readLines),
    file_b_contents = map(file_b, readLines),
    file_a_lines = map2_chr(file_a_contents, line_a, ~ .x[.y + c(0:10)] %>% paste(collapse = "\n")),
    file_b_lines = map2_chr(file_b_contents, line_b, ~ .x[.y + c(0:10)] %>% paste(collapse = "\n"))
  ) %>%
  select(-c(file_a_contents, file_b_contents))
head(y)

y %>% group_by(a_file, b_file) %>%
  summarise(n = n()) %>% View()

y %>% filter(a_file == "credit bureau data.R") %>%
  select(b_file, file_a_lines, file_b_lines)

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
use_data(example_x, example_y)
# If the DESCRIPTION contains LazyData: true, then datasets will be lazily loaded.
# This means that they won’t occupy any memory until you use them.

# Adding 'R' to Depends field in DESCRIPTION
# Creating 'data/'
# Setting LazyData to 'true' in 'DESCRIPTION'
# Saving 'x', 'y' to 'data/x.rda', 'data/y.rda'
# Document your data (see 'https://r-pkgs.org/data.html')

# Often, the data you include in data/ is a cleaned up version of raw data you’ve gathered from elsewhere.
# I highly recommend taking the time to include the code used to do this in the source version of your package.
# This will make it easy for you to update or reproduce your version of the data.
# I suggest that you put this code in data-raw/.
# You don’t need it in the bundled version of your package, so also add it to .Rbuildignore.
# Do all this in one step with:
usethis::use_data_raw()

# You can see this approach in practice in some of my recent data packages.
# I’ve been creating these as packages because the data will rarely change, and because multiple packages can then use them for examples:
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
# @format gives an overview of the dataset.
# For data frames, you should include a definition list that describes each variable.
# It’s usually a good idea to describe variables’ units here.
#
# @source provides details of where you got the data, often a \url{}.
#
# Never @export a data set.


# Documentation: ------------------------------------------------------------
# Use Ctrl + . in RStudio and start typing “DESCRIPTION” to activate a helper that makes it easy to open a file for editing.
# In addition to a filename, your hint can be a function name.
# This is very handy once a package has lots of functions and files below R/.
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
# devtools now includes functions (test_file() and test_coverage_file()) to improve development of a single file.
# Rather than running all tests, or manually supplying a filter argument to restrict the tests test_file() automatically runs the corresponding tests for a given source file.
# These functions make the feedback loop when developing new features quicker as you only run the relevant tests for the file you are editing.
# This requires you use a standard naming convention for your tests, e.g. if you have a source file R/featureA.R the corresponding test file would be tests/testthat/test-featureR.R.
# The tests file to run is automatically detected from the open file in RStudio (if available), so you can call test_file() with either the source file or the test file open.
# A corresponding test_coverage_file() function shows the test code coverage for a single source file.
# There is also a test_coverage() function to report test coverage for your whole package.
# These functions have RStudio addins which allows you to bind them to shortcut keys.
revdep_check()



# style R code ------------------------------------------------------------
# 7.3 Code style
# We recommend following the tidyverse style guide (https://style.tidyverse.org), which goes into much more detail than we can here.
# Its format also allows it to be a more dynamic document than this book.

# Although the style guide explains the “what” and the “why”, another important decision is how to enforce a specific code style.
# For this we recommend the styler package (https://styler.r-lib.org); its default behaviour enforces the tidyverse style guide.
# There are many ways to apply styler to your code, depending on the context:

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
#
# The use of Git or another version control system is optional, but a recommended practice in the long-term. We explain its importance in 18.
# For example, it’s nerve-wracking and somewhat dangerous to apply a function like styler::style_pkg() without some way to see exactly what changed and to accept/reject such changes in a granular way.
#
# The styler package can also be integrated with various platforms for hosting source code and doing continuous integration.
# For example, the tidyverse packages use a GitHub Action that restyles a package when triggered by a special comment (/style) on a pull request.
# This allows maintainers to focus on reviewing the substance of the pull request, without having to nitpick small issues of whitespace or indentation1516.
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
# Create a file for package installation:
create_dependencies_file()
# This creates a 'dependencies.R' in the 'inst' folder


# ??NAMESPACE -------------------------------------------------------------
# namespace::namespace-package		Namespace managment functions
# namespace::registerNamespace		Create, register, obtain a reference to, and unregister namespaces

# OpenMx::imxGenerateNamespace		imxGenerateNamespace
# OpenMx::omxCheckNamespace		omxCheckNamespace

# pkgload::ns_env		Return the namespace environment for a package.
# pkgload::parse_ns_file		Parses the NAMESPACE file for a package

# pkgmaker::getLoadingNamespace		Namespace Development Functions

# ProjectTemplate::.attach.or.add.namespace		Attach a package or add a namespace

# Rango::forceAssignMyNamespace		Function to force the assignment of new items in the Rango namespace Adapted from Rcpp
# Rcpp::populate		Populates a namespace or an environment with the content of a module
# rdrop2::get_dropbox_token		Retrieve oauth2 token from rdrop2-namespaced environment

# redland::librdf_parser_get_namespaces_seen_count		Get the number of namespaces seen during parsing
# redland::librdf_parser_get_namespaces_seen_prefix		Get the prefix of namespaces seen during parsing
# redland::librdf_parser_get_namespaces_seen_uri		Get the uri of namespaces seen during parsing
# redland::librdf_serializer_set_namespace		Set a namespace URI/prefix mapping.
# redland::mergeNamespace_roclet		A custom Roxygen roclet that adds Redland RDF functions to NAMESPACE file generated by Roxygen.
# redland::roclet_output.roclet_mergeNamespace		Roxygen output function that merges a base NAMESPACE file with the Roxygen dynamically created NAMSPACE file
# redland::roclet_process.roclet_mergeNamespace		Roxygen process function for the 'mergeNamespace' roclet
# redland::setNameSpace		Set a namespace for the serializer.

# reproducible::.requireNamespace		Provide standard messaging for missing package dependencies

# Rfast::AddToNamespace		Insert/remove function names in/from the NAMESPACE file
# Rfast::checkNamespace		Check Namespace and Rd files
# Rfast::sourceR		Source many R files

# RGtk2::gFileAttributeMatcherEnumerateNamespace		gFileAttributeMatcherEnumerateNamespace
# RGtk2::gFileInfoHasNamespace		gFileInfoHasNamespace
# RGtk2::gFileQueryWritableNamespaces		gFileQueryWritableNamespaces
# RGtk2::atkActionIfaceDoAction		Virtual Methods

# rlang::call_name		Extract function name or namespace of a call
# rlang::is_namespace		Is an object a namespace environment?
# rlang::ns_env		Get the namespace of a package

# roxygen2::namespace_roclet		Roclet: make 'NAMESPACE'
# roxygen2::roxygenize		Process a package with the Rd, namespace and collate roclets.

# rsdmx::findNamespace		findNamespace
# rsdmx::getNamespaces		getNamespaces
# rsdmx::namespaces.SDMX		namespaces.SDMX

# RSiteCatalyst::GetTrackingServer		Get Tracking Server Associated with a Namespace (Company)
# RWsearch::p_unload_all		Unload all non-base and non-recommended packages from the namespace
# RxODE::rxReq		Require namespace, otherwise throw error.

# shiny::NS		Namespaced IDs for inputs/outputs
# shinyAce::.tools		Get namespace to get access to unexported functions, namely RdTags
# shinyAce::.utils		Get namespace to get access to unexported functions, namely .getHelpFile .assignLinebuffer .assignEnd .guessTokenFromLine .completeToken

# sinew::makeImport		Scrape R script to create namespace calls for R documentation
# sinew::pretty_namespace		Append namespace to functions in script

# SpaDES.core::.isNamespaced		Check is module uses module namespacing
# SpaDES.core::.callingModuleName		Namespacing within 'SpaDES'

# storr::join_key_namespace		Recycle key and namespace
# TAM::TAM-utilities		Utility Functions in 'TAM'

# testit::test_pkg		Run the tests of a package in its namespace
# tinkr::md_ns		Aliased namespace prefix for commonmark
# tune::load_pkgs		Quietly load package namespace

# usethis::use_namespace		Use a basic 'NAMESPACE'

# withr::with_package		Execute code with a modified search path

# ztable::.onAttach		Hooks for Namespace Events

# base::.subset		Internal Objects in Package 'base'
# base::environment		Environment Access
# base::.onLoad		Hooks for Namespace Events
# base::asNamespace		Namespace Internals
# base::attachNamespace		Loading and Unloading Name Spaces
# base::getExportedValue		Namespace Reflection Support
# utils::assignInNamespace		Utility Functions for Developing Namespaces



# document ----------------------------------------------------------------
# Run R CMD check on a Package Directory
document:::check_package()

check_package(
  package_directory,
  working_directory,
  check_as_cran = TRUE,
  stop_on_check_not_passing = FALSE,
  debug = TRUE
)


# rcmdcheck ---------------------------------------------------------------
library(rcmdcheck)
##
(my_check <- rcmdcheck::rcmdcheck())
(my_check_details <- check_details(my_check))

cat(my_check_details$warnings)
my_check_details$warning[2] %>% cat()
my_check_details$warning[4] %>% cat()
my_check_details$warning[5] %>% cat()
my_check_details$warning[6] %>% cat()
my_check_details$warning[7] %>% cat()

cat(my_check_details$notes)
my_check_details$notes[1] %>% cat()
my_check_details$notes[2] %>% cat()

my_check_details$session_info$platform
# A named list with elements:
#
# - package: package name.
# - version: package version.
# - description: the contents of the DESCRIPTION file of the package. A single string.
#
# - notes: character vector of check NOTEs, each NOTE is an element.
# - warnings: character vector of check WARNINGs, each WARNING is an element.
# - errors: character vector of check ERRORs, each ERROR is an element. A check timeout adds an ERROR to this vector.
#
# - platform: check platform
# - checkdir: check directory. the path to the check directory, if it hasn't been cleaned up yet, or NA. The check directory is automatically cleaned up, when the check object is deleted (garbage collected).
#
# - install_out: the output of the installation, contents of the 00install.out file. A single string.
# - session_info: the output of sessioninfo::session_info(), from the R session performing the checks.
#
# - cran: whether it is a CRAN packaged package.
# - bioc: whether it is a BioConductor package.
##

getOption("repos") #"https://cran.rstudio.com/"
Sys.getenv("RCMDCHECK_ERROR_ON") #[1] ""
rcmdcheck(
  path = ".",
  quiet = FALSE,
  args = character(),
  build_args = character(),
  check_dir = NULL,
  libpath = .libPaths(),
  repos = getOption("repos"),
  timeout = Inf,
  error_on = Sys.getenv("RCMDCHECK_ERROR_ON", c("never", "error", "warning", "note")[1]),
  env = character()
)


# check_details	Query R CMD check results and parameters
# compare_checks	Compare a set of check results to another check result

# compare_to_cran	Compare a check result to CRAN check results
# cran_check_flavours	Download and show all CRAN check flavour platforms
# cran_check_results	Download and parse R CMD check results from CRAN

# parse_check	Parse 'R CMD check' results from a file or string
# parse_check_url	Shorthand to parse R CMD check results from a URL

# print.rcmdcheck	Print R CMD check results
# print.rcmdcheck_comparison	Print R CMD check result comparisons

# rcmdcheck	Run R CMD check from R and Capture Results
# rcmdcheck-config	rcmdcheck configuration
# rcmdcheck_process	Run an R CMD check process in the background

# xopen.rcmdcheck	Open the check directory in a file browser window

# {devtools} --------------------------------------------------------------
missing_s3()

# bash	Open bash shell in package directory.

# build	Build package

# build_manual	Create package pdf manual
# build_readme	Build a Rmarkdown files package
# build_rmd	Build a Rmarkdown files package
# build_site	Execute 'pkgdown' build_site in a package
# build_vignettes	Build package vignettes.
build_vignettes(
  pkg = ".",
  dependencies = "VignetteBuilder",
  clean = TRUE,
  upgrade = "never",
  quiet = F,
  install = TRUE,
  keep_md = TRUE
)

# Build and check a package, cleaning up automatically on success:
# check
# check_built

# check_mac_release	Check macOS package

# check_man	Check documentation, as R CMD check does.

# check_rhub	Run CRAN checks for package on R-hub

# check_win	Build windows binary package.
# check_win_devel	Build windows binary package.
# check_win_oldrelease	Build windows binary package.
# check_win_release	Build windows binary package.

# clean_vignettes	Clean built vignettes.

# create	Create a package

# dev_mode	Activate and deactivate development mode.

# dev_sitrep	Report package development situation

# document	Use roxygen to document a package.

# install	Install a local development package.
# install_deps	Install package dependencies if needed.
# install_dev_deps	Install package dependencies if needed.

# lint	Lint all source files in a package.

# load_all	Load complete package

# release	Release package to CRAN.

# reload	Unload and reload package.
# run_examples	Run all examples in a package.
# save_all	Save all documents in an active IDE session.
# show_news	Show package news

# source_gist	Run a script on gist
# source_url	Run a script through some protocols such as http, https, ftp, etc.

# spell_check	Spell checking

# test	Execute testthat tests in a package
# test_active_file	Execute testthat tests in a package
# test_coverage	Execute testthat tests in a package
# test_coverage_active_file	Execute testthat tests in a package

# uninstall	Uninstall a local development package.

# wd	Set working directory.

# {attachment} ------------------------------------------------------------
tmpdir <- tempdir()
file.copy(system.file("dummypackage", package = "attachment"), tmpdir, recursive = T)
dummypackage <- file.path(tmpdir, "dummypackage")
browseURL(dummypackage)
#`NAMESPACE`:
## Generated by roxygen2: do not edit by hand
# export(my_mean)
# importFrom(magrittr,"%>%")
# importFrom(stats,na.omit)

att_from_namespace(path = file.path(dummypackage, "NAMESPACE"))
# i Loading dummypackage
# Writing NAMESPACE
# Writing NAMESPACE
# [1] "magrittr"

browseURL(dummypackage)
#`NAMESPACE`:
## Generated by roxygen2: do not edit by hand
# export(my_mean)
# importFrom(magrittr,"%>%")

#' my_mean
#'
#' @param x a vector
#'
#' @export
#' @importFrom magrittr %>%
my_mean <- function(x){
  x <- x %>% stats::na.omit()
  1+1
  sum(x)/base::length(x)
}

#cleanup
unlink(dummypackage)

# bookdown Imports are in Rmds
# imports <- c("bookdown", attachment::att_from_rmds("."))
# attachment::att_to_desc_from_is(path.d = "DESCRIPTION", imports = imports, suggests = NULL)

# Return all dependencies from... (att_from_*)
# -description, namespace, rmd(s), rscript(s)
att_from_description() %>% find_remotes()
att_from_namespace()
att_from_rscripts()
att_from_rmds() # Vignettes?

# Remotes (e.g., github)
# find_remotes	  Proposes values for Remotes field for DESCRIPTION file based on your installation
# set_remotes_to_desc	  Add Remotes field to DESCRIPTION based on your local installation

# Create a file for package installation:
create_dependencies_file()
# This creates a 'dependencies.R' in the 'inst' folder

# Amend DESCRIPTION with dependencies read from package code parsing:
att_amend_desc()
att_to_desc_from_pkg()
# Amend DESCRIPTION with dependencies from imports and suggests package list:
att_to_desc_from_is()


# install_from_description	Install missing package from DESCRIPTION
# install_if_missing	install packages if missing



# {CMD} ---------------------------------------------------------------------
# Utility Functions in CDM
## requireNamespace with package message for needed installation
CDM_require_namespace(pkg)
## attach internal function in a package
cdm_attach_internal_function(pack, fun)

## print function in summary
cdm_print_summary_data_frame(obji, from=NULL, to=NULL, digits=3, rownames_null= F)
## print summary call
cdm_print_summary_call(object, call_name="call")
## print computation time
cdm_print_summary_computation_time(object, time_name="time", time_start="s1",
                                   time_end="s2")

## string vector of matrix entries
cdm_matrixstring( matr, string )

## mvtnorm::rmvnorm with vector conversion for n=1
CDM_rmvnorm(n, mean=NULL, sigma, ...)
## fit univariate and multivariate normal distribution
cdm_fit_normal(x, w)

## fit unidimensional factor analysis by unweighted least squares
cdm_fa1(Sigma, method=1, maxit=50, conv=1E-5)

## another rbind.fill implementation
CDM_rbind_fill( x, y )
## fills a vector row-wise into a matrix
cdm_matrix2( x, nrow )
## fills a vector column-wise into a matrix
cdm_matrix1( x, ncol )

## SCAD thresholding operator
cdm_penalty_threshold_scad(beta, lambda, a=3.7)
## lasso thresholding operator
cdm_penalty_threshold_lasso(val, eta )
## ridge thresholding operator
cdm_penalty_threshold_ridge(beta, lambda)
## elastic net threshold operator
cdm_penalty_threshold_elnet( beta, lambda, alpha )
## SCAD-L2 thresholding operator
cdm_penalty_threshold_scadL2(beta, lambda, alpha, a=3.7)
## truncated L1 penalty thresholding operator
cdm_penalty_threshold_tlp( beta, tau, lambda )
## MCP thresholding operator
cdm_penalty_threshold_mcp(beta, lambda, a=3.7)

## general thresholding operator for regularization
cdm_parameter_regularization(x, regular_type, regular_lam, regular_alpha=NULL,
                             regular_tau=NULL )
## values of penalty function
cdm_penalty_values(x, regular_type, regular_lam, regular_tau=NULL,
                   regular_alpha=NULL)
## thresholding operators regularization
cdm_parameter_regularization(x, regular_type, regular_lam, regular_alpha=NULL,
                             regular_tau=NULL)

## utility functions for P-EM acceleration
cdm_pem_inits(parmlist)
cdm_pem_inits_assign_parmlist(pem_pars, envir)
cdm_pem_acceleration( iter, pem_parameter_index, pem_parameter_sequence, pem_pars,
                      PEM_itermax, parmlist, ll_fct, ll_args, deviance.history=NULL )
cdm_pem_acceleration_assign_output_parameters(res_ll_fct, vars, envir, update)

## approximation of absolute value function and its derivative
abs_approx(x, eps=1e-05)
abs_approx_D1(x, eps=1e-05)

## information criteria
cdm_calc_information_criteria(ic)
cdm_print_summary_information_criteria(object, digits_crit=0, digits_penalty=2)

## string pasting
cat_paste(...)



# {collidr} ---------------------------------------------------------------
library(collidr)

# Check for Namespace Collisions:
CRAN_collisions(function_or_package_name, CRANdf)
# function_or_package_name: A character string, or vector of character strings.
# CRANdf: Optionally provide an updated CRAN data.frame (obtain with getCRAN())

# Test single function name
function_or_package_name <- "a3.r2"
CRAN_collisions(function_or_package_name)
# Test multiple function names
function_or_package_name <- c("a3.r2", "xtable.A3")
CRAN_collisions(function_or_package_name)

# Test single package name
function_or_package_name <- "dplyr"
CRAN_collisions(function_or_package_name)
# Test multiple package names
function_or_package_name <- c("dplyr", "data.frame", "gsubfn")
CRAN_collisions(function_or_package_name)

##
# Test single function name
function_name <- "a3.r2"
CRAN_function_collisions(function_name)
# Test multiple function names
function_name <- c("a3.r2", "xtable.A3")
CRAN_function_collisions(function_name)

# Test single package name
package_name <- "dplyr"
CRAN_package_collisions(package_name)
# Test multiple package names
package_name <- c("dplyr", "data.frame", "gsubfn")
CRAN_package_collisions(package_name)




# {roxygen2} --------------------------------------------------------------




# {origin} ----------------------------------------------------------------
# origin: Explicitly Qualifying Namespaces by Automatically Adding 'pkg::' to Functions
# To learn more about origin, start with the vignette: 'browseVignettes(package = "origin")'
# Useful links: https://github.com/mnist91/origin
##
originize_file(
  file = "testscript.R",
  pkgs = .packages(),
  overwrite = TRUE,
  ask_before_applying_changes = TRUE,
  ignore_comments = TRUE,
  check_conflicts = TRUE,
  add_base_packages = FALSE,
  check_base_conflicts = TRUE,
  check_local_conflicts = TRUE,
  excluded_functions = list(
    dplyr = c("%>%", "across"),
    data.table = c(":=", "%like%"),
    # exclude from all packages:
    c("first", "last")
  ),
  verbose = TRUE,
  use_markers = TRUE
)
# Other versions of the above:
originize_dir()	#Originize a complete directory
originize_pkg()	#Originize a Package Project

##
originize_selection()	#Wrapper function to be used as an RStudio addin

##
# Also:

# Get All Exported Functions From a Package
origin::get_exported_functions("base")
# Find All User Defined functions in the Project
origin::get_local_functions() #path = "."

# Get Packages from the DESCRIPTION file
origin::get_pkgs_from_description()


# + Addins:
# Besides using regular R functions to originize files, there are also useful addins delivered with origin. These addins are designed to be used on-the-fly while coding. You can either originize selected text, the currently opened file, or all scripts in the currently opened project.
# However, to have as much control as when using functions, each function argument corresponds to an option that can be set and used inside the addins, e.g.
options(origin.pkgs = c("dplyr", "data.table"), origin.overwrite = TRUE)
# Actually, most function arguments of origin first check whether an option has been declared and uses the assigned value as its default. This allows for equal outcomes regardless whether you use the addin or a function sequentially.

# Safety Measures:
# Since origin changes files on disk, it is very important that the user has full control over what happens and user input is required before critical steps.

# Logging:
# Most importantly, the user must be aware of what the originized file(s) would look like. For this, all changes and potential missed changes are presented, either in the Markers tab (recommended) or in the console.
# - insertion: pkg:: is inserted prior to a function
# - missing: an object that has the same name as a function but not undoubtedly used as a function. In R it is usually no problem to have variables that name like functions (data or df are popular examples). While it is always clear when a function is directly used as one, functions can also be arguments of other functions, most famously in functional programming like the *apply family or purrr. origin highlights such cases in the logging output.
# - infix: functions like %>% are exported by packages but cannot be called with the pkg::fun() convention. Such functions are highlighted by default to point the user that these stem from a package. When using dplyr-style code, consider to exclude the pipe-operator via exclude_functions.

# Same Function Name in Multiple Packages:
# Due to the variety of R packages, function names must not be unique across all packages out there. By default, R masks priorly imported functions by those imported afterwards. origin mimics this rule by applying a higher priority to those packages that are listed first. In case there is a conflict regarding a used function,
# These functions are listed along with the packages from which they stem.
#
#     Used functions in mutliple Packages!
#     filter: dplyr, stats first: data.table, dplyr
#     Order in which relevant packges are evaluated: data.table >> dplyr >> stats
#     Do you want to proceed? 1: YES 2: NO
##

# Custom Functions Mask Exported Functions:
# As packages mask each others functions, the same applies to locally defined custom functions. In case you defined your own last function in your project, origin should not add dplyr:: to it. Therefore, your project is searched for function definitions and local functions have higher priority than those exported by packages. Note that, depending on the project size, this process can take quite some time. In this case, set the argument/option path_to_local_functions to a subdirectory or check_local_conflicts to FALSE to skip this feature.
#
# Locally defined and used functions mask exported functions from packages
#
# last: dplyr
#
# Local functions have higher priority. In case you want to use an exported version of a function listed above set pkg::fun manually
#
# Got it? 1: YES 2: NO 3: Show files
#
# Many Files Selected
# When originizing a complete folder or project, many R scripts might be checked. In case the user is unaware that there are many files in the selected folder, resulting in a long run time of origin, a warning is triggered and user input is required.
#
# You are about to originize 99 files.
#
# Proceed? 1: YES 2: NO 3: Show files
#
# Final Check:
# Before the proposed changes are applied eventually, a final user input is required.
# Happy with the result? 😀
# 1: YES 2: NO
#
# Discussion:
# Whether or not to add pkg:: to each (imported) function is a controversial issue in the R community. While the tidyverse style guide does not mention explicit namespacing, R Packages and the Google R style guide are in favor of it.
#
# Pros
# -very explicit
# -completely avoid namespace conflicts
# -no need to attach the complete namespace of a package
# -keep track of which function belongs to which package
#
# Cons
# -(minimal) performance issue
# -more writing required
# -longer code
# -infix functions like %>% cannot be called via magrittr::%>% and workarounds are still required here. Either use
# library(magrittr, include.only = "%>%")
# `%>%` <- magrittr::`%>%`
#
# -calling library() on top of a script clearly indicates which packages are needed.
# A not yet installed package throws an error right away, not until a function cannot be found later in the script.
# However, one can use the include_only argument and set it to NULL.
# No functions are attached into the search list then.
#
# library(magrittr, include_only = NULL)
##




# {various utils pkgs} ----------------------------------------------------------------
###
library(hutils)
# seems like a pretty cool collection of functions!
# Shorthand for requireNamespace
RQ("dplyr", "dplyr needs installing")

# Alias for if (!requireNamespace(pkg, quietly = TRUE)) yes else no.
# Typical use-case would be RQ(pkg, install.packages("pkg"))].
#
# Default values for yes and no from hutils v1.5.0.
# This function is not recommended for use in scripts as it is a bit cryptic;
# its use-case is for bash scripts and the like where calls like this would otherwise be frequent and cloud the message.

###
library(mvbutils)
make.NAMESPACE() #Auto-create a NAMESPACE file
##
# Package mvbutils is a collection of utilities offering the following main features:
# - Hierarchical organization of projects (AKA tasks) and sub-tasks, allowing switching within a single R session, searching and moving objects through the hierarchy, objects in ancestor tasks always visible from child (sub)tasks, etc. See cd.
# - Improved function, text, and script editing facilities, interfacing with whichever text editor you prefer. The R command line is not frozen while editing, and you can have multiple edit windows open. Scriptlets can be edited as expressions, for subsequent calls to eval. Function documentation can be stored as plain text after the function definition, and will be found by help even if the function isn't part of a package. There is also a complete automatic text-format backup system for functions & text. See fixr.
# - Automated package construction, including production of Rd-format from plain text documentation. Packages can be edited & updated while loaded, without needing to quit/rebuild/reinstall. See mvbutils.packaging.tools.
# - "Lazy loading" for individual objects, allowing fast and transparent access to collections of biggish objects where only a few objects are used at a time. See mlazy.
# - Miscellaneous goodies: local/nested functions (mlocal), display of what-calls-what (foodweb), multiple replacement (multirep), nicely-formatted latex tables (xtable.mvb), numerous lower-level lower-level utility functions and operators (mvbutils.utils, mvbutils.operators, extract.named, mcut, search.for.regexpr, strip.missing, FOR )
#
# To get the full features of the mvbutils package– in particular, the project organization– you need to start R in the same directory every time (your "ROOT task"), and then switch to whichever project from inside R; see cd.
# Various options always need to be set to make fixr and the debug package work the way you want, so one advantage of the start-in-the-same directory-approach is that you can keep all your project-independent options(), library loads, etc., in a single .First function or ".Rprofile" file, to be called automatically when you start R.
# However, many features (including support for the debug package) will work even if you don't follow this suggestion.
# The remaining sections of this document cover details that most users don't know about; there's no need to read them when you are just starting out with mvbutils.
#
# Housekeeping info
# On loading, the mvbutils package creates a new environment in the search path, called mvb.session.info, which stores some housekeeping information. mvb.session.info is never written to disk, and disappears when the R session finishes. [For Splus users: mvb.session.info is similar to frame 0.] You should never change anything in mvb.session.info by hand, but it is sometimes useful to look at some of the variables there:
# .First.top.search is the directory R started in (your ROOT task).
# .Path shows the currently-attached part of the task hierarchy.
# base.xxx is the original copy of an overwritten system function, e.g. library
# fix.list keeps track of objects being edited via fixr
# session.start.time is the value of Sys.time() when mvbutils was loaded
# source.list is used by source.mvb to allow nesting of sources
# r.window.handle is used by the handy package (Windows only)
# partial.namespaces is used to alleviate difficulties with unloadable data files– see mvbutils.packaging.tools
# things whose name starts with ".." are environments used in live-editing packages
# maintained.packages is a list of the latter
#
# Redefined functions
# On loading, package mvbutils redefines a few system functions: lockEnvironment, importIntoEnv loadNamespace, print.function, help, rbind.data.frame and, by default, library, savehistory, loadhistory, and save.image. (The original version of routine xxx can always be obtained via base.xxx if you really need it.) The modifications, which are undone when you unload mvbutils, should have [almost] no side-effects. Briefly:
# library is modified so that its default pos argument is just under the ROOT workspace (the one that was on top when mvbutils was loaded), which is needed by cd. This means that packages no longer get attached by default always in position 2.
# lockEnvironment and importIntoEnv are modified to allow live-editing of your own maintained packages– no change to default behaviour.
# loadNamespace has the default value of its "partial" argument altered, to let you bypass .onLoad for selected faulty packages– see mvbutils.packaging.tools and look for partial.namespaces. This allows the loading of certain ".RData" files which otherwise crash from hidden attempts to load a namespace. It lets you get round some truly horrendous problems arising from faults with 3rd-party packages, as well as problems when you stuff up your own packages.
# rbind.data.frame does not ignore zero-row arguments (so it takes account of their factor levels, for example).
# rbind.data.frame: dimensioned elements (i.e. matrices & arrays within data.frames) no longer have any extra attributes removed. Hence, for example, you can (if you are also using my nicetime package) rbind two data frames that both have POSIXct-matrix elements without turning them into raw seconds and losing timezones.
# help and ? are modified so that, if utils:::help can't find help for a function (but not a method, dataset, or package), it will look instead for a doc attribute of the function to display in a pager/browser using dochelp. Character objects with a ".doc" extension will also be found and displayed. This lets you write and distribute "informal help".
# loadhistory and savehistory are modified so that they use the current "R_HISTFILE" environment variable if it set. This can be set dynamically during an R session using Sys.setenv. Standard R behaviour is to respect "R_HISTFILE" iff it is set before the R session starts, but not to track it during a session. If "R_HISTFILE" is not set, then cd will on first use set "R_HISTFILE" to "<<ROOT task>>/.RHistory", so that same the history file will be used throughout each and every session.
# save.image is modified to call Save instead; this will behave exactly the same for workspaces not using mvbutils task-hierarchy feature or the debug package, but otherwise will prevent problems with mtraced functions and mlazyed objects.
# print.function is modified to let you go on seamlessly using functions written prior to R 2.14 in conjunction with the srcref system imposed by R 2.14; see fixr.
#
# Some of these redefinitions are optional and can be turned off if you really want: loadhistory, savehistory, save.image, library, lockEnvironment, importIntoEnv, and loadNamespace. To turn them off, set options(mvbutils.replacements=FALSE) before loading mvbutils. However, I really don't recommend doing so; it will prevent cd etc, fixr, and the package-maintenance tools from working properly, and if you use debug you will probably cause yourself trouble when you forgetfully save.image an mtraced function. You can also set the "mvbutils.replacements" option to a character vector comprising some or all of the above names, so that only those happen; if so, you're on your own. The other replacements are unavoidable (but should not be apparent for packages that don't import mvbutils).
# After mvbutils has loaded, you can undo the modification of a function xxx by calling assign.to.base( "xxx", base.xxx). Exceptions are help, ?, print.function, rbind.data.frame which are intrinsic to mvbutils. Unloading mvbutils' will undo all the modifications.
#
# Nicer posixt behaviour
# POSIXct etc have some nasty behaviour, and mvbutils used to include some functions that ameliorated things. I've moved them into a separate package nicetime, available on request.
#
# Ess and mvbutils
# For ESS users: I'm not an Emacs user and so haven't tried ESS with the mvbutils package myself, but a read-through of the ESS documentation (as of ~2005) suggests that a couple of ESS variables may need changing to get the two working optimally. Please check the ESS documentation for further details on these points. I will update this helpfile when/if I receive more feedback on what works (though there hasn't been ESS feedback in ~8 years...).
# cd changes the search list, so you may need to alter "ess-change-sp-regex" in ESS.
# cd also changes the prompt, so you may need to alter "inferior-ess-prompt". Prompts have the form WORD1/WORD2/.../WORDn> where WORDx is a letter followed by zero or more letters, underscores, periods, or digits.
# move can add/remove objects in workspaces other than the top one, so if ESS relies on stored internal summaries of "what's where", these may need updating.
#
# Display bugs
# If you have a buggy Linux display where readline() always returns the cursor to the start of the line, overwriting any prompt, then try options( cd.extra.CR=TRUE).
##
# as.cat( x)
# clip( x, n=1)
# cq( ...)
# deparse.names.parsably( x)
# empty.data.frame( ...)
# env.name.string( env)
# expanded.call( nlocal=sys.parent())
# everyth( x, by=1, from=1)
# find.funs(pos=1, ..., exclude.mcache = TRUE, mode="function")
# find.lurking.envs(obj, delve=FALSE, trace=FALSE)
# index( lvector)
# integ(expr, lo, hi, what = "x", ..., args.to.integrate = list())
# is.dir( dir)
# isF( x)
# isT( x)
# legal.filename( name)
# lsall( ...)
# masked( pos)
# masking( pos=1)
# mkdir( dirlist)
# most.recent( lvec)
# my.all.equal( x, y, ...)
# named( x)
# nscat( fmt, ..., sep='\n', file='')
# nscatn( fmt, ..., sep='\n', file='')
# option.or.default( opt.name, default=NULL)
# pos( substrs, mainstrs, any.case = FALSE, names.for.output)
# put.in.session( ...)
# returnList( ...)
# safe.rbind( df1, df2) # Deprecated in 2013
# scatn( fmt, ..., sep='\n', file='')
# to.regexpr( x)
# yes.no( prompt, default)
###


