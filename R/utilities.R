# notin `%notin%` <- Negate(`%in%`)

#' nCores
#' @export
#'
how_many_cores_you_got <- function() {
  nCores <- parallel::detectCores(logical = F) - 1
  return(nCores)
}

#' @title Copy a dataframe or matrix to clipboard
#'
#' @param x the dataframe or matrix
#' @param row.names copy the row names? TRUE by default
#' @param col.names copy the column names? TRUE by default
#' @param ... dots argument
#'
#' @export
copy.table <- function(x, row.names = TRUE, col.names = TRUE, ...) {
  write.table(x, "clipboard-256", sep = "\t", row.names = row.names, col.names = col.names, ...)
}
# copy <- function(x, row.names = FALSE, col.names = TRUE, ...) {
#   write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
#   #write.table(x, file = "clipboard-16384", sep = "\t", row.names = row.names, col.names = col.names, ...)
# }

#' @title luna
#' alias for length(unique(na.omit(x)))
#'
#' @param x x
#'
#' @export
luna <- function(x) {
  length(unique(stats::na.omit(x)))
}

# na.omit:
# function (object, ...) {
#   if(!is.atomic(object)) return(object)
#   d <- dim(object)
#   if (length(d) > 2L) return(object)
#   omit <- seq_along(object)[is.na(object)]
#   if (length(omit) == 0L) return(object)
#
#   if (length(d)) {
#     omit <- unique(((omit - 1)%%d[1L]) + 1L)
#     nm <- rownames(object)
#     object <- object[-omit, , drop = FALSE]
#   }
#   else {
#     nm <- names(object)
#     object <- object[-omit]
#   }
#   if (any(omit > 0L)) {
#     names(omit) <- nm[omit]
#     attr(omit, "class") <- "omit"
#     attr(object, "na.action") <- omit
#   }
#   object
# }



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
    names(Filter(type, mget(ls(all = T))))
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

#----Formatting----
#' comma
#' @param x x
#' @export
comma <- function(x){
  format(x, digits = 2, big.mark = ",")
}

#' capwords
#' @param s string
#' @param strict TF
#' @export
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#### NOTES ####
###
#> embody.expr:
# env <- list(`~` = identity)
# body(e) <- eval(call("substitute", body(e), env), envir = baseenv())
##
#
#
# func.temp <- function(data) {
#   #qrObj <- qr(as.matrix(x))
#   x <- data %>% keep(is.numeric) %>% names()
#   y <- names(data)[names(data) %notin% x]
#   combin_mat <- expand.grid(x, y, stringsAsFactors = F)
#   #y <- data %>% discard(is.numeric)
#   combin_mat %>%
#     mutate(is_nested = map2_lgl(combin_mat$Var1, combin_mat$Var2, function(x, y) {
#       if(length(unique(data[, x])) == 1) return(NA)
#       if(length(unique(data[, y])) == 1) return(NA)
#       M <- xtabs(~ data[, x] + data[, y], sparse = T)
#       all(Matrix::colSums(M > 0) == 1L)
#     })) %>% filter(is_nested == TRUE) %>%
#     mutate(adist = diag(adist(Var1, Var2, ignore.case = T, useBytes = T,partial = T)))
#   #round(apply(M, 1, function(x) x / Matrix::colSums(M)), 2) %>% t()
#   #Matrix::Diagonal(x = Matrix::rowSums(M)) %*% (1*(M!=0)) %*% Matrix::Diagonal(x = Matrix::colSums(M))
# }
#
# rowPerc <-
#   function(tab)  {
#     if (length(dim(tab))>1) {#tab is a two-way table
#
#
#       rperc <- round(100 * apply(tab, 2, function(y) y / rowSums(tab)), 2)
#       rperc <- cbind(rperc,rep(100,nrow(tab)))
#       rownames(rperc) <- rownames(tab)
#       colnames(rperc) <- c(colnames(tab),"Total")
#       rperc2 <- as.table(rperc)
#       names(dimnames(rperc2)) <- names(dimnames(tab))
#       return(rperc2)
#     } else {
#       rperc <- round(100*tab/sum(tab),2)
#       rperc <- as.matrix(rperc)
#       rperc2 <- rbind(rperc,100)
#       rperc2 <- t(rperc2)
#       colnames(rperc2) <- c(rownames(rperc),"Total")
#       rownames(rperc2) <- ""
#       names(dimnames(rperc2)) <- c(names(dimnames(tab)),"")
#       return(rperc2)
#     }
#   }
#

########
# # this function does the actual work for all of the enumLC methods
# internalEnumLC <- function(qrObj, ...)
# {
#   R <- qr.R(qrObj)                     # extract R matrix
#   numColumns <- dim(R)[2]              # number of columns in R
#   rank <- qrObj$rank                   # number of independent columns
#   pivot <- qrObj$pivot                 # get the pivot vector
#
#   if (is.null(numColumns) || rank == numColumns)
#   {
#     list()                            # there are no linear combinations
#   } else {
#     p1 <- 1:rank
#     X <- R[p1, p1]                    # extract the independent columns
#     Y <- R[p1, -p1, drop = FALSE]     # extract the dependent columns
#     b <- qr(X)                        # factor the independent columns
#     b <- qr.coef(b, Y)                # get regression coefficients of
#     # the dependent columns
#     b[abs(b) < 1e-6] <- 0             # zap small values
#
#     # generate a list with one element for each dependent column
#     lapply(1:dim(Y)[2],
#            function(i) c(pivot[rank + i], pivot[which(b[,i] != 0)]))
#   }
# }
#
# {
#   lcList <- enumLC(x)
#   initialList <- lcList
#   badList <- NULL
#   if(length(lcList) > 0)
#   {
#     continue <- TRUE
#     while(continue)
#     {
#       # keep removing linear dependencies until it resolves
#       tmp <- unlist(lapply(lcList, function(x) x[1]))
#       tmp <- unique(tmp[!is.na(tmp)])
#       badList <- unique(c(tmp, badList))
#       lcList <- enumLC(x[,-badList, drop = FALSE])
#       continue <- (length(lcList) > 0)
#     }
#   } else badList <- NULL
#   list(linearCombos = initialList, remove = badList)
# }
#
