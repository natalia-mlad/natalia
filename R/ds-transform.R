#----Numeric Tricks----
# data <- as.data.frame(df[,c(4:29)])

#' polytomize data to demo ordinal models
#' @param x ?a vector?
#' @export
polytomize_data <- function(x) {
  thresholds <- c(-Inf, -0.5, 1, Inf) # 3 categories
  ordLong <- data.frame(lapply(exLong[-1], function(x) {
    cut(x, breaks = thresholds, labels = FALSE)
  }))
  ordLong$ID <- 1:nrow(ordLong)
  return(ordLong)
}

#' removes leading zero before decimals and rounds number to two decimals
#' @param x numeric vector
#' @param k number of decimals
#' @returns character vector
#' @export
nozero <- function(x, k = 2) {
  sub('^(-)?0[.]', '\\1.', format(round(x, k), nsmall = k))
}

#' formats numbers to no more or less than two decimals
#' @param x x
#' @param k number of decimals
#' @export
zero <- function(x, k = 2) {
  format(round(x, k), nsmall = k)
}

#' remap.distance
#'
#' @param data data
#' @param message TF
#'
#' @export
remap.distance <- function(data, message = TRUE) {
  if(missing(data)) missingMsg('dat')
  ind <- seq_len(ncol(data))
  nms <- colnames(data)
  if(is.null(nms)) nms <- paste0("Item ", ind)
  ret <- sapply(ind, function(i, data, nms, message){
    x <- data[,i]
    s <- sort(unique(x))
    se <- min(s, na.rm = TRUE):max(x, na.rm = TRUE)
    if(length(s) != length(se)){
      if(message) message(sprintf('\"%s\" re-mapped to ensure all categories have a distance of 1', nms[i]))
      for(i in 2L:length(s)) x <- ifelse(x == s[i], se[i], x)
    }
    x
  }, data = data, nms = nms, message = message)
  rownames(ret) <- rownames(data)
  colnames(ret) <- colnames(data)
  ret
}

#' change.range.simple
#'
#' @param data data
#' @param col_names names
#' @param min num
#' @param max num
#'
#' @export
change.range.simple <- function(data, col_names, min = 0, max = 100) {
  stopifnot(min != max)
  stopifnot(is.numeric(data[, col_names]))
  stopifnot(is.character(col_names))

  min.new <- min
  max.new <- max
  new.range <- max.new - min.new

  min.old <- min(data[, col_names], na.rm = TRUE)
  max.old <- max(data[, col_names], na.rm = TRUE)
  old.range <- max.old - min.old

  data[, col_names] <- ((new.range * (data[, col_names] - min.old)) / old.range) + min.new
  #plot(density(y)); plot(density(data[, col_names]))
  #new_col_names <- paste0(col_names, "_new_range")
}

#' change.range
#'
#' @param object obj
#' @param new_data new
#' @param ... dot
change.range <- function (object, new_data, ...) {
  # prep.range <- function (x, training, info = NULL, ...) {
  #   col_names <- eval_select_recipes(x$terms, training, info)
  #   check_type(training[, col_names])
  # }
  #function (x, training, info = NULL, ...) {
  #head(data[, sapply(data, is.numeric)])
  #vapply(data[, sapply(data, is.numeric)], min, c(min = 0), na.rm = TRUE)
  #without the c(min = 0): Error argument "FUN.VALUE" is missing, with no default
  min.old <- vapply(training[, col_names], min, c(min = 0), na.rm = TRUE)
  max.old <- vapply(training[, col_names], max, c(max = 0), na.rm = TRUE)
  old.range <- max.old - min.old
  #max.new <- c(100,100)
  #min.new <- c(0,0)
  new.range <- max.new - min.new #b - a
  y <- ((new.range*(x - min.old)) / old.range) + min.new
  #normx = 100*(x - minx)/(maxx - minx) .
  #Y = (max(Y) - min(Y))*(X - min(X))/(max(X) - min(X)) + min(Y)


  # step_range_new(terms = x$terms, role = x$role, trained = TRUE,
  #                min = x$min, max = x$max, ranges = rbind(mins, maxs),
  #                skip = x$skip, id = x$id)
  # tmp <- as.matrix(new_data[, colnames(object$ranges)])
  # tmp <- sweep(tmp, 2, object$ranges[1, ], "-")
  # tmp <- tmp * (object$max - object$min)
  # tmp <- sweep(tmp, 2, object$ranges[2, ] - object$ranges[1, ], "/")
  # tmp <- tmp + object$min
  # tmp[tmp < object$min] <- object$min
  # tmp[tmp > object$max] <- object$max
  # tmp <- tibble::as_tibble(tmp)
  # new_data[, colnames(object$ranges)] <- tmp
  # as_tibble(new_data)
}

#' Test.Nest
#'
#' @param x x
#' @param y y
#'
#' @export
test.nest <- function(x, y) {
  M <- stats::xtabs(~ x + y, sparse = T)
  result <- all(Matrix::colSums(M > 0) == 1L)
  if (result == TRUE) {
    print(paste(deparse(substitute(y)),
                "is nested in",
                deparse(substitute(x))))
  } else if (result == FALSE) {
    print(paste(
      deparse(substitute(y)),
      "is NOT nested in",
      deparse(substitute(x))
    ))
  }
}


#' spearman_brown
#'
#' @param df dataframe
#' @param items items
#' @param name character
#' @param SB_only TRUE/FALSE; default is false
#'
#' @export
#'
spearman_brown <- function(df, items, name = "", SB_only = FALSE) {
  cor_value <- stats::cor.test(magrittr::extract2(df, items[1]), magrittr::extract2(df, items[2]), na.rm = T)$estimate
  SB_value <- (abs(cor_value) * 2)/(1 + abs(cor_value))
  if (SB_only) {
    return(SB_value)
  }
  result <- data.frame(correlation = cor_value, spearman_brown = SB_value, row.names = name)
  return(result)
}

#' t-test
#'
#' @param n n
#' @param loc loc
#' @param scale scale
ttest <- function(n, loc, scale) {
  # generate sample:
  sample <- stats::rnorm(n, loc, scale)
  # calculate test statistic:
  stat <- sqrt(n) * mean(sample) / stats::sd(sample)
  # get test decision:
  decision <- abs(stat) > 1.96
  # return result:
  return(list("decision" = decision))
}

#----Fuzzy Matching----
# x <- inner_join(dobname2, dobname, by = c(dateOfBirth = "DateOfBirth"))
# x <- x %>% group_by(id) %>% filter(adist == min(adist))
#
# x %>% group_by(id) %>% filter(adist == min(adist)) %>% group_by(FullName.x, FullName.y) %>% summarise()
##
#matched4 <- matched4[, names(matched4) %in% c("id", "CustomerID")]
##
# Unit Test:
##
#dobname2$name2 <- "" # Creating an empty column
#output <- matrix(ncol=5, nrow=nrow(cb_raw2))
##

#' Join two tables using fuzzy matching
#'
#' Fuzzy loop
#'
#' @param df.x df
#' @param df.y df
#' @param by var
#' @param max.distance num
#' @param ignore.case TF
#' @param useBytes TF
#'
#' @export
fuzzy.join <- function(df.x, df.y, by = NULL, max.distance = 0.3,
                       ignore.case = TRUE, useBytes = TRUE) {
  x_names <- names(df.x)
  y_names <- names(df.y)
  # parsing the `by` argument:
  #(e.g., by = c("x" = "y"))
  if (is.list(by)) {
    by <- by[c("x", "y")]
  }
  else if (is.character(by)) {
    by_x <- names(by)
    by_y <- unname(by)
    by_x[by_x == ""] <- by_y[by_x == ""]
    by <- list(x = by_x, y = by_y)
  } else {
    stop("by must be a (named) character vector, list, or NULL")
    #abort("by must be a (named) character vector, list, or NULL")
    #warning(""); return(NULL)
  }
  #check_join_vars(by$x, x_names)
  stopifnot(by$x %in% x_names)
  stopifnot(by$y %in% y_names)
  # create named integers:
  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$y)
  stopifnot(class(df.x[[x_by]]) == "character")
  stopifnot(class(df.y[[y_by]]) == "character")
  #x$adist <- adist(x$FullName.x, x$FullName.y, ignore.case = T, useBytes = T, partial = T) %>% diag()
  df.x$JOIN <- ""
  for (i in 1:nrow(df.x)) {
    temp <- agrep(df.x[[x_by]][i], df.y[[y_by]], value = TRUE, max.distance = max.distance,
                  ignore.case = ignore.case, useBytes = useBytes)
    temp <- paste0(temp, "")
    df.x$JOIN[i] <- temp
  }
  percent_coverage <- sum(is.na(df.x$JOIN))/nrow(df.x)
  if(percent_coverage > 0.33) {
    warning(paste0("More than a third of the variables (", round(percent_coverage * 100),
                   "%) were unmatched! Consider increasing the max.distance or changing other agrep options."))

    # df.x2 <- df.x %>% filter(is.na(JOIN))
    # vector.max.distance <- seq(max.distance, 5 + max.distance, by = 0.2)
    # #vector.max.distance <- c(0.02, 0.05, 5) #seq(0.02, 5, length.out = 11)
    # while(percent_coverage < 0.33) {
    #   for (i in 1:nrow(df.x)) {
    #     temp <- agrep(df.x[[x_by]][i], df.y[[y_by]], value = TRUE, max.distance = max.distance, ignore.case = ignore.case, useBytes = useBytes)
    #     temp <- paste0(temp, "")
    #     df.x$JOIN[i] <- temp
    #   }
    #   percent_coverage <- sum(is.na(df.x$JOIN))/nrow(df.x)
    # }
  }
  #cat(deparse(substitute(df.x)))
  cat(crayon::cyan(crayon::bold(paste0("Fuzzy matching is completed.\nYou can now join datasets \`",
                                       match.call()[2], "\` (the output of this function) and \`",
                                       match.call()[3], "\` on \"JOIN\" = \"", names(y_by), "\" \n"))))
  #items <- left_join(items, df.x, by = c("question_text" = "join"))
  #kamoa <- full_join(kamoa, items, by = c("join" = "question_text"))
  #scoring_table <- full_join(scoring_table, questions, by = c("join" = "Statement"))
  return(df.x)
}

#### fuzzy notes: ####
# my.fun <- function(df.x, df.y){
#   x_by = c(kamoa = 2)
#   y_by = c(question_text = 2)
#   df.x[,"JOIN"] <- "" #NA
#   #cat(crayon::cyan(sys.call()))
#   #cat(crayon::cyan(eval(deparse(substitute(df.x)))))
#   #cat(crayon::cyan(quote(df.x)))
#   #cat(crayon::cyan(df.y))
#   for (i in 1:nrow(df.x)) {
#     temp <- agrep(df.x[[x_by]][i], df.y[[y_by]], value = TRUE, max.distance = 0.3,
#                   ignore.case = T, useBytes = T)
#     temp <- paste0(temp, "")
#     df.x$JOIN[i] <- temp
#   }
#   cat(crayon::cyan(match.call()[2]))
#   cat(crayon::green(match.arg()))
#   #cat(deparse(substitute(df.x)))
#   #cat(crayon::cyan(crayon::bold(paste0("Fuzzy matching is completed. You can now join ", substitute(df.x), " and ", deparse(substitute(df.y))))))
# }
# my.fun(kamoa, items)
# ##
# (df.x = kamoa)
# df.x$JOIN <- ""
# df.x
# ##
# df.x = kamoa
# df.x[,"JOIN"] <- ""
# df.x
###
# my_list <- list()
# for(i in 1:nrow(df.x)) {
#   temp <- agrep(df.x[[x_by]][i], df.y[[y_by]], max.distance = 0.05, ignore.case = T, useBytes = T, value = T)
#   temp <- paste0(temp, "")
#   my_list[[i]] <- temp
#   df.x$JOIN[i] <- temp
# }
###
#df.x[[x$key]][i], df.y[[y$key]]
##
#x_loc <- set_names(seq_along(x_names), x_names)
#vars <- list(x = list(key = x_by, out = x_loc), y = list(key = y_by, out = y_loc))
#x_in <- as_tibble(df.x, .name_repair = "minimal")
#y_in <- as_tibble(df.y, .name_repair = "minimal")
#x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
#y_key <- set_names(y_in[vars$y$key], names(vars$y$key))
##
# check_join_vars <- function(vars, names) {
#   if (!is.character(vars)) abort("join columns must be character vectors.")
#   na <- is.na(vars)
#   if (any(na)) abort(c("Join columns must be not NA.", x = glue("Problem at position {err_vars(na)}.")))
#   dup <- duplicated(vars)
#   if (any(dup)) abort(c("Join columns must be unique.", x = glue("Problem at position {err_vars(dup)}.")))
#   missing <- setdiff(vars, names)
#   if (length(missing) > 0) abort(c("Join columns must be present in data.", x = glue("Problem with {err_vars(missing)}.")))
# }
###
# #function (x, y, by = NULL)
# #join_mutate(x, y, by = by, type = "inner")
# vars <- join_cols(tbl_vars(x), tbl_vars(y), by = by)
# na_equal <- check_na_matches(na_matches)
# x_in <- as_tibble(x, .name_repair = "minimal")
# y_in <- as_tibble(y, .name_repair = "minimal")
# x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
# y_key <- set_names(y_in[vars$y$key], names(vars$y$key))
# rows <- join_rows(x_key, y_key, type = type, na_equal = na_equal)
# x_out <- set_names(x_in[vars$x$out], names(vars$x$out))
# y_out <- set_names(y_in[vars$y$out], names(vars$y$out))
# if (length(rows$y_extra) > 0L) {
#   x_slicer <- c(rows$x, rep_along(rows$y_extra, NA_integer_))
#   y_slicer <- c(rows$y, rows$y_extra)
# } else {
#   x_slicer <- rows$x
#   y_slicer <- rows$y
# }
# out <- vec_slice(x_out, x_slicer)
# out[names(y_out)] <- vec_slice(y_out, y_slicer)
# if (!keep) {
#   key_type <- vec_ptype_common(x_key, y_key)
#   out[names(x_key)] <- vec_cast(out[names(x_key)], key_type)
#   if (length(rows$y_extra) > 0L) {
#     new_rows <- length(rows$x) + seq_along(rows$y_extra)
#     out[new_rows, names(y_key)] <- vec_cast(vec_slice(y_key, rows$y_extra), key_type)
#   }
# }
# dplyr_reconstruct(out, x)}


# Simulate Data -----------------------------------------------------------
#' @title Simulate data from the model.
#' @description Use a continuous covariate x.
#' @return A data frame with the following columns.
#'   * `y`: Simulated normal responses.
#'   * `x`: A simulated covariate of zeroes and ones.
#'   * `beta_true`: The value of the regression coefficient `beta`
#'     used to simulate the data.
#' @export
#' @examples
#' if(FALSE) {
#' simulate_data_continuous()
#' }
#'
simulate_data_continuous <- function() {
  alpha <- stats::rnorm(1, 0, 1)
  beta <- stats::rnorm(1, 0, 1)
  sigma <- rhcauchy(1, 1)
  x <- stats::rnorm(100, 1, 1) # continuous covariate
  y <- stats::rnorm(100, alpha + x * beta, sigma)
  sim <- basename(tempfile(pattern = "sim"))
  tibble(x = x, y = y, beta_true = beta, sim = sim)
}

#' @title Simulate data from the model.
#' @description Use a discrete covariate x.
#' @return A data frame with the following columns.
#'   * `y`: Simulated normal responses.
#'   * `x`: A simulated covariate of zeroes and ones.
#'   * `beta_true`: The value of the regression coefficient `beta`
#'     used to simulate the data.
#' @export
#' @examples
#' if(FALSE){
#' simulate_data_discrete()
#' }
#'
simulate_data_discrete <- function() {
  alpha <- stats::rnorm(1, 0, 1)
  beta <- stats::rnorm(1, 0, 1)
  sigma <- rhcauchy(1, 1)
  x <- stats::rbinom(100, 1, 0.5) # discrete covariate
  y <- stats::rnorm(100, alpha + x * beta, sigma)
  sim <- basename(tempfile(pattern = "sim"))
  tibble(x = x, y = y, beta_true = beta, sim = sim)
}


#### generate poly terms of a data matrix / data frame ######
#' Generate poly terms of a data matrix / data frame
#'
#' @param xdata the dataframe (only predictor variables). Factors with more than two levels should not be inputted as integers.
#' @param deg the max degree of polynomial terms.
#' @param maxInteractDeg the max degree of dummy and nondummy predictor variable interaction terms
#' @param Xy the dataframe with the response in the final column (provide xdata or Xy but not both). Factors with more than two levels should not be inputted as integers.
#' @param model_formula Internal use. Formula used to generate the training model matrix.
#'     Note: anticipates that polynomial terms are generated using internal functions of library(polyreg)
#'     so YMMV if the formula is not generated on the training data by get_poly().
#'     Also, providing model_formula bypasses deg and maxInteractDeg.
#' @param standardize standardize all continuous variables? (Default: FALSE.)
#' @param intercept Include intercept? Default: FALSE.
#'
#' @return a model matrix, with the model formula as an additional attribute
#' @export
#'
#' @examples
#' if(FALSE) {
#' X = get_poly(mtcars, 2)
#' W = get_poly(Xy=mtcars, 2) # treats final column as response
#' ncol(W) < ncol(X)          # TRUE
#' X_train <- get_poly(mtcars[1:20,], 4, 2)
#' X_test <- get_poly(mtcars[21:32,], model_formula = attributes(X_train)$formula)
#' }
get_poly <- function(xdata = NULL, deg = 1, maxInteractDeg = deg, Xy = NULL,
                     model_formula = NULL, standardize = FALSE, intercept = FALSE) {
  if(sum(is.null(xdata) + is.null(Xy)) != 1)
    stop("please provide get_poly() xdata or Xy (but not both).")

  W <- if(is.null(xdata)) as.data.frame(Xy) else as.data.frame(xdata)
  if(standardize) {
    to_z <- which(unlist(lapply(W, is_continuous)))
    W[, to_z] <- scale(W[, to_z])
  }

  if(is.null(model_formula)) {
    x_cols <- 1:(ncol(W) - is.null(xdata))
    y_name <- if(is.null(xdata)) colnames(Xy)[ncol(Xy)] else NULL
    remove(xdata)
    remove(Xy)

    # coerce binary or character variables into factors
    W_distincts <- N_distinct(W)
    to_factor <- which((W_distincts == 2) | unlist(lapply(W, is.character)))
    #x_factors <- vector("logical", length(x_cols))
    for(i in to_factor)
      W[, i] <- as.factor(W[, i])
    x_factors <- if(ncol(W) > 1) unlist(lapply(W[,x_cols], is.factor)) else is.factor(W)
    P_factor <- sum(x_factors)

    factor_features <- c()
    # stores individual levels of factor variables, omitting one as reference
    # e.g. for a variable "sex" coded as binary, just "male" will be stored
    # this enables the appropriate formula to be written that handles interactions
    # suppose the variable party had three levels the strings
    # 'party == GOP' and 'party == independent' are stored
    # and democrat is the reference...

    for(i in which(x_factors)){

      if(W_distincts[i] > 2){
        tmp <- paste(colnames(W)[i], "==", paste0("\'", levels(W[,i])[-1], "\'"))
        tmp <- paste0("(", tmp, ")")
        factor_features <- c(factor_features, tmp)
      } else{
        factor_features <- c(factor_features, colnames(W)[i])
      }
    }

    continuous_features <- cf <- colnames(W)[x_cols][!x_factors]
    P_continuous <- length(continuous_features)

    P <- P_continuous + P_factor
    # P does not reflect intercept, interactions, or polynomial terms

    for(i in 2:deg)
      continuous_features <- c(continuous_features, paste("pow(", cf, ",", i, ")"))
    # pow() is a helper function that deals with the nuissance
    # that lm(y ~ x + x^2 + x^3)
    # will only estimate one slope but we want three...
    # the string above will be used to make the appropriate formula
    # y ~ x + pow(x, 2) + pow(x, 3)

    features <- c(continuous_features, factor_features)

    if(maxInteractDeg > 1 && ncol(W) > 1)
      features <- get_interactions(features, maxInteractDeg, names(x_factors[x_factors]))

    if(length(features) > nrow(W))
      warning("P > N. With polynomial terms and interactions, P is ",
              length(features))

    model_formula <- stats::as.formula(paste0(y_name, " ~ ",
                                       ifelse(intercept, "", "-1 +"),
                                       paste(features, collapse=" + ")))

  }
  return(model_matrix(model_formula, W))
}
