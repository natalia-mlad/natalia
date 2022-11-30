

#' nCores
#' @export
how_many_cores_you_got <- function() {
  nCores <- parallel::detectCores(logical = F) - 1
  return(nCores)
}

#' Copy a dataframe or matrix to clipboard
#'
#' @param x the dataframe or matrix
#' @param row.names copy the row names? TRUE by default
#' @param col.names copy the column names? TRUE by default
#' @param ... dots argument passed on to [utils::write.table()]
#'
#' @export
copy_table <- function(x, row.names = TRUE, col.names = TRUE, ...) {
  write.table(x, "clipboard-256", sep = "\t", row.names = row.names, col.names = col.names, ...)
}
# copy <- function(x, row.names = FALSE, col.names = TRUE, ...) {
#   write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
#   #write.table(x, file = "clipboard-16384", sep = "\t", row.names = row.names, col.names = col.names, ...)
# }

#' @title luna
#' alias for length(unique(na.omit(x)))
#' @param x x
#' @export
luna <- function(x) {
  length(unique(stats::na.omit(x)))
}

#' @title suna
#' alias for sort(unique(na.omit(x)))
#' @param x x
#' @export
suna <- function(x) {
  sort(unique(stats::na.omit(x)))
}

#' letters_to_numbers
#'
#' @param x character vector
#'
#' @return interger vector
#' @export
#'
#' @examples
#' letters_to_numbers("a")
#' letters_to_numbers(c("p", "n"))
letters_to_numbers <- function(x) {
  # stopifnot(length(x) > 0)
  stopifnot(is.character(x))
  letter <- tolower(x)
  myLetters <- letters[1:26]
  match(letter, myLetters)
}




# environments ------------------------------------------------------------

#' Load data to a separate environment
#'
#' @param env new.env
#' @param RData path for the .RData file
#'
#' @export
#'
#' @examples
#' if(FALSE) {
#' march2021.data <- LoadToEnvironment("March 2021.RData")
#' dataset <- march2021.data$dataset
#' }
LoadToEnvironment <- function(RData, env = new.env()) {
  stopifnot(is.character(RData))
  stopifnot(grepl("[.]RData$", RData, ignore.case = T))
  load(RData, env)
  return(env)
}


#' my_env
#' @param type type e.g., is.data.frame, is.character,
my_env <- function(type = NULL) {
  if (is.null(type)) {
    #TODO: how to make sure it actually works in my environmnent and
    #not the functions env....
    ls.str()
  } else {
    stopifnot(is.character(type))
    # TODO; make sure it's formatted so: is.data.frame, is.character,
    names(qdap::Filter(type, mget(ls(all = T))))
  }
}


#' unattach_all
#'
#' @return NULL
#' @export
#'
unattach_all <- function() {
  #invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
  ##
  # lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  # invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))
  ##
  invisible(suppressMessages(suppressWarnings(lapply(c("gsl","fBasics","stringr","stringi","Rmpfr"), require, character.only = TRUE))))
  invisible(suppressMessages(suppressWarnings(lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE))))
  sessionInfo()

  #the above is a test

  invisible(lapply(paste0('package:', c("stringr","fBasics")), detach, character.only=TRUE,unload=TRUE))
  #In the line above, I have inserted by hand what I know the package dependencies to be. A user must know this a priori or have their own automated
  #method to discover it. Without removing dependencies first, the user will have to cycle through loading namespaces and then detaching otherPkgs a
  #second time through.
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE,unload=TRUE))

  bspkgs.nb<-sessionInfo()$basePkgs[sessionInfo()$basePkgs!="base"]
  bspkgs.nbu<-bspkgs.nb[bspkgs.nb!="utils"]
  names(bspkgs.nbu)<-bspkgs.nbu
  suppressMessages(invisible(lapply(paste0('package:', names(bspkgs.nbu)), detach, character.only=TRUE,unload=TRUE)))

  #again this thoroughly removes all packages and loaded namespaces except for base packages "base" and "utils" (which is highly not recommended).
}



#----Wrangling w/ Functions----
#' embody expression for a function
#'
#' notes:
#' env <- list(`~` = identity)
#' body(e) <- eval(call("substitute", body(e), env), envir = baseenv())
#'
#' @param expr expr
#'
#' @export
embody.expr <- function (expr) {
  fun <- function() {}
  body(fun) <- expr
  fun
}

#' find function dependencies
#' @param e expression
#' @param merge TF
#' @export
find.dependencies <- function(e, merge = F) {
  #how to deal with a function requiring a package to run??
  substitute(e) %>% embody.expr() %>% codetools::findGlobals(merge = merge)
}
