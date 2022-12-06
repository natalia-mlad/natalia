#' @title luna
#' alias for `length(unique(na.omit(x)))`
#' @param x a vector
#' @export
luna <- function(x) {
  length(unique(stats::na.omit(x)))
}

#' @title suna
#' alias for `sort(unique(na.omit(x)))`
#' @param x a vector
#' @export
suna <- function(x) {
  sort(unique(stats::na.omit(x)))
}

#' Copy a dataframe or matrix to clipboard
#'
#' @param x the dataframe or matrix
#' @param row.names copy the row names? TRUE by default
#' @param col.names copy the column names? TRUE by default
#' @param clip_size the size limit of the clipboard (in Kb); `2^14` by default.
#' This would only need to be increased if the `object.size(x)/1024` exceeds this default.
#' Please be mindful of the total memory on your machine.
#' @param ... dots argument passed on to [utils::write.table()]
#' @export
#'
#' @examples
#' \dontrun{
#' copy_table(mtcars)
#' }
copy_table <- function(x, row.names = TRUE, col.names = TRUE, clip_size = 2^14, ...) {
  stopifnot(is.numeric(clip_size))
  write.table(
    x,
    file = paste0("clipboard-", clip_size),
    sep = "\t",
    row.names = row.names,
    col.names = col.names,
    ...
  )
}

#' How many cores you got? A lot!
#' Shortcut for finding the number of cores for parallel computing.
#' @export
how_many_cores_you_got <- function() {
  nCores <- parallel::detectCores(logical = FALSE) - 1
  return(nCores)
}


# Environments ------------------------------------------------------------
#' Load data to a separate environment
#' Useful for having a peak of the objects in an .RData file and only importing a select few.
#'
#' @param RData path for the .RData file
#' @param env the environment to load to; creates a new one by default
#' @export
#'
#' @examples
#' \dontrun{
#' march2021 <- LoadToEnvironment("March 2021.RData")
#' dataset <- march2021$dataset
#' rm(march2021)
#' }
LoadToEnvironment <- function(RData, env = new.env()) {
  stopifnot(is.character(RData))
  stopifnot(grepl("[.]RData$", RData, ignore.case = T))
  # TODO: check path is real
  load(RData, env)
  return(env)
}


#' Unattach All Packages
#' Detaches all packages, except for the base packages `base` and `utils`.
#' It is highly recommended to not detach these.
#' @param base_keep character vector of the base packages to not detach; keeps `base` and `utils` by default.
#' @returns `sessionInfo()` invisibly
#' @export
unattach_all <- function(base_keep = c("base", "utils")) {
  unattach_internal(names(sessionInfo()$otherPkgs))
  # TODO: check dependendencies? cycle 2nd time through?
  # TODO: what about "loaded via a namespace (and not attached)"?
  # TODO: warning (or Are You Sure?) if(!all(c("base", "utils") %in% base_keep))
  base_remove <- sessionInfo()$basePkgs[!sessionInfo()$basePkgs %in% base_keep]
  unattach_internal(base_remove)
  return(invisible(sessionInfo()))
}

#' @keywords internal
unattach_internal <- function(pkgs) {
  # if (!is.null(names(pkgs))) pkgs <- names(pkgs)
  paste0('package:', pkgs) %>%
    lapply(detach, character.only = TRUE, unload = TRUE) %>%
    invisible() %>%
    suppressMessages()
}


# Wrangling with Functions ------------------------------------------------
#' embody expression for a function
#' @param expr an expression
#' @export
embody.expr <- function(expr) {
  fun <- function() {}
  body(fun) <- expr
  return(fun)
}

#' find a function's dependencies
#' @param e an expression
#' @param merge logical; FALSE by default; passed onto [codetools::findGlobals]
#' @export
find.dependencies <- function(e, merge = FALSE) {
  # TODO: how to deal with a function requiring a package to run?
  substitute(e) %>%
    embody.expr() %>%
    codetools::findGlobals(merge = merge)
}
