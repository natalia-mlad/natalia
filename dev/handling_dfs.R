
allClasses <- c("character", "factor", "numeric", "integer", "logical", "Date")
preChecks = c("isKey", "isSingular", "isSupported")
nvariables <- ncol(data)
n <- nrow(data)
vnames <- names(data)
if (ordering == "alphabetical") {
  index <- order(names(data))
} else index <- 1:nvariables

for (idx in index) {
  #idx = 29
  v <- data[[idx]]
  vnam <- vnames[idx]
  preCheckRes <- lapply(preChecks, function(x) eval(call(x, v)))
  preCheckProblems <- sapply(preCheckRes, function(x) x$problem)
  preCheckMessages <- sapply(preCheckRes, function(x) x$message)
  checkRes <- check(v, checks = setChecks(character = characterChecks, factor = factorChecks, labelled = labelledChecks, 
                                          haven_labelled = havenlabelledChecks, numeric = numericChecks, integer = integerChecks, logical = logicalChecks, Date = dateChecks),
                    nMax = maxProbVals, maxDecimals = maxDecimals, ...)
  problems <- sapply(checkRes, function(x) x[[1]])
  
  ####
  if (any(unlist(c(problems, preCheckProblems)))) {
    y <- ifelse(output == "pdf", "$\\times$", "&times;")
    allRes$problems[allRes$variable == vnam] <- y
  }
  if (onlyProblematic && (!any(preCheckProblems) && !any(problems))) skip <- TRUE
  if (!skip) {
    printable_name <- gsub("_", "\\\\_", vnam)
    writer("## ", printable_name, "\n", outfile = vListConn)
    extraLinkCharBegin <- "["
    extraLinkCharEnd <- "]"
    if (!includeVariableList) {
      extraLinkCharBegin <- extraLinkCharEnd <- ""
    }
    allRes$name[allRes$variable == vnam] <- paste(extraLinkCharBegin, printable_name, extraLinkCharEnd, sep = "")
    allRes$label[allRes$variable == vnam] <- ifelse(is.null(attr(v, "label", exact = TRUE)), "", attr(v, "label", exact = TRUE))
    allRes$description[allRes$variable == vnam] <- ifelse(is.null(attr(v, "shortDescription")), "", attr(v, "shortDescription"))
    allRes$vClass[allRes$variable == vnam] <- oClass(v)[1]
    allRes$missingPct[allRes$variable == vnam] <- paste(format(round(100 * mean(is.na(v)), 2), nsmall = 2), "%")
    allRes$distinctVals[allRes$variable == vnam] <- length(unique(v))
    if (!is.null(attr(v, "label", exact = TRUE))) {
      writer("*", attr(v, "label", exact = TRUE), "*\n", outfile = vListConn)
    }
    if (any(preCheckProblems)) {
      writer(paste("* ", preCheckMessages[preCheckProblems], "\n", collapse = " \n ", sep = ""), outfile = vListConn)
    }
    else {
      if (extraMessages$do) writer(paste("* ", extraMessages$messages, "\n", collapse = " \n ", sep = ""), outfile = vListConn)
      if (doSummarize) sumTable <- pander::pandoc.table.return(summarize(v, reportstyleOutput = TRUE, summaries = setSummaries(character = characterSummaries, factor = factorSummaries, labelled = labelledSummaries, haven_labelled = havenlabelledSummaries, numeric = numericSummaries, integer = integerSummaries, logical = logicalSummaries, Date = dateSummaries), maxDecimals = maxDecimals, ...), justify = "lr")
      if (doVisualize) visual <- visualize(v, vnam, doEval = FALSE, visuals = setVisuals(character = characterVisual, factor = factorVisual, labelled = labelledVisual, haven_labelled = havenlabelledVisual, numeric = numericVisual, integer = integerVisual, logical = logicalVisual, Date = dateVisual), ...)
      chunk_name <- paste0("Var-", idx, "-", stringi::stri_trans_general(gsub("[_:. ]", "-", vnam), "Latin-ASCII"))
      if (twoCol) {
        twoCols.wrapper(sumTable, visual, label = chunk_name, outfile = vListConn)
      }
      else {
        if (doSummarize) writer(sumTable, outfile = vListConn)
        if (doVisualize) fig.wrapper(visual, label = chunk_name, outfile = vListConn)
        writer("\n", outfile = vListConn)
      }
      if (doCheck) {
        if (any(problems)) {
          messages <- sapply(checkRes, function(x) x[[2]])[problems]
          for (i in 1:length(messages)) {
            writer(paste0("- ", messages[i], " \n"), outfile = vListConn)
          }
        }
      }
    }
  }
}

###
#dataReporter:::check.character %>% View()
#dataReporter:::identifyMissing.character %>% View()
#View(dataReporter:::identifyMissingCF)
#View(dataReporter:::identifyMissNumber)
check <- function (v, nMax = 10, checks = setChecks(), ...) 
  UseMethod("check")

check.character <- function (v, nMax = 10, checks = setChecks(), characterChecks = NULL, ...) {
  if (is.null(characterChecks)) characterChecks <- checks$character
  out <- lapply(characterChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- characterChecks
  out
}

identifyMissingCF <- function (v, nMax) {
  v <- na.omit(as.character(v))
  problem <- FALSE
  problemValues <- NULL
  missStrs <- c("", "nan", "NaN", "NAN", "na", "NA", "Na", 
                "Inf", "inf", "-Inf", "-inf", "-")
  missStrsOcc <- intersect(v, missStrs)
  missSpaceOcc <- identifyMissRepChar(v, " ")
  missDotPrefixOcc <- unique(v[substr(v, 1, 1) == "."])
  v <- v[!(v %in% c(missStrsOcc, missSpaceOcc, missDotPrefixOcc))]
  if (identifyNums(v)$problem) {
    v <- as.numeric(v)
    missAllNinesOcc <- identifyMissNumber(v, 9, FALSE)
    missAllEightsOcc <- identifyMissNumber(v, 8, FALSE)
  }
  else {
    missAllNinesOcc <- identifyMissNumber(v, 9, TRUE)
    missAllEightsOcc <- identifyMissNumber(v, 8, TRUE)
  }
  allProblemOcc <- c(missStrsOcc, missAllNinesOcc, missAllEightsOcc, 
                     missSpaceOcc, missDotPrefixOcc)
  if (length(allProblemOcc) > 0) {
    problemValues <- allProblemOcc
    problem <- TRUE
  }
  outMessage <- messageGenerator(list(problem = problem, problemValues = problemValues), 
                                 message = identifyMissingMessage, nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, 
                   problemValues = problemValues))
}




isKey <- function (v) {
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  if (length(unique(v)) == length(v) & !any(class(v) %in% c("numeric", "integer", "Date"))) {
    out$problem <- TRUE
    out$message <- "The variable is a key (distinct values for each observation)."
  }
  #Convert a list resulting from the checks performed in a checkFunction into a checkResult
  #object, thereby supplying it with a print() method.
  checkResult(out)
}

checkResult <- function(ls) {
  if (length(setdiff(names(ls), c("problem", "message", "problemValues"))) != 0) {
    stop("The inputted list does not qualify as a checkResult")
  }
  else {
    class(ls) <- "checkResult"
  }
  ls
}

isSingular <- function (v) {
  lV <- length(v)
  if (any(c("labelled", "haven_labelled") %in% class(v))) 
    v <- dataReporter_as_factor(v)
  v <- na.omit(v)
  pctMiss <- round(100 * (lV - length(v))/lV, 2)
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  nVals <- length(unique(v))
  if (nVals <= 1) {
    allNA <- nVals == 0
    val <- ifelse(allNA, "NA", as.character(v[1]))
    out$problem <- TRUE
    out$message <- paste("The variable only takes one ", 
                         ifelse(allNA, "", "(non-missing) "), "value: ", 
                         printProblemValues(val), ".", ifelse(allNA, "", 
                                                              paste(" The variable contains", pctMiss, "\\% missing observations.")), 
                         sep = "")
  }
  checkResult(out)
}

isSupported <- function (v) {
  suppClasses <- c("character", "factor", "labelled", "haven_labelled", "numeric", "integer", "logical", "Date")
  vClasses <- class(v)
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  if (any(vClasses %in% suppClasses)) {
    return(checkResult(out))
  }
  out$problem <- TRUE
  out$message <- paste("The variable has class", vClasses[1], "which is not supported by dataReporter.")
  checkResult(out)
}


###
matrix(
  c("Number of observations", "Number of variables", n, nvariables), 2,
  dimnames = list(NULL, c("Feature", "Result"))
)

allRes <-
  data.frame(
    variable = vnames[index],
    name = rep(NA, nvariables),
    vClass = rep(NA, nvariables),
    distinctVals = rep(NA, nvariables),
    missingPct = rep(NA, nvariables),
    problems = rep("", nvariables),
    stringsAsFactors = FALSE,
    label = rep(NA, nvariables),
    description = rep(NA, nvariables)
  )

#######
checks = setChecks()
characterChecks <- checks$character
factorChecks <- checks$factor
labelledChecks <- checks$labelled
havenlabelledChecks <- checks$haven_labelled
numericChecks <- checks$numeric
integerChecks <- checks$integer
logicalChecks <- checks$logical
dateChecks <- checks$Date
###
writer <- function(x, ..., outfile = fileConn, sep = "\n") {
  cat(paste0(x, ...), file = outfile, append = TRUE, sep = sep)
}
chunk.wrapper <- function(x, ..., outfile = fileConn, options = c("echo=FALSE", "warning=FALSE"), label = NULL) {
  writer(paste0("```{r ", ifelse(is.null(label), ", ",
                                 paste0("'", label, "', ")), paste0(options, collapse = ", "),
                "}"), outfile = outfile)
  writer(x, ..., outfile = outfile)
  writer("```\n", outfile = outfile)
}
fig.wrapper <- function(x, outfile = fileConn, options = c("echo=FALSE", "fig.width=4", "fig.height=3", "message=FALSE", "warning=FALSE"), label = NULL) {
  chunk.wrapper(x, outfile = outfile, options = options,
                label = label)
}
secretChunk.wrapper <- function(x, ..., outfile = fileConn, options = c("echo=FALSE", "include=FALSE", "warning=FALSE", "message=FALSE", "error=FALSE"), label = NULL) {
  chunk.wrapper(x, outfile = outfile, options = options,
                label = label)
}
twoCols.wrapper <- function(text, figure, outfile = fileConn, outputty = output, label = NULL) {
  if (outputty == "pdf") {
    writer("\\bminione", outfile = outfile)
    writer(text, outfile = outfile)
    writer("\\emini", outfile = outfile)
    writer("\\bminitwo", outfile = outfile)
    fig.wrapper(figure, label = label, outfile = outfile)
    writer("\\emini", outfile = outfile)
  }
  if (outputty == "html") {
    writer("<div class = \"row\">", outfile = outfile)
    writer("<div class = \"col-lg-8\">", outfile = outfile)
    writer(text, outfile = outfile)
    writer("</div>", outfile = outfile)
    writer("<div class = \"col-lg-4\">", outfile = outfile)
    fig.wrapper(figure, label = label, outfile = outfile)
    writer("</div>", outfile = outfile)
    writer("</div>", outfile = outfile)
  }
  writer("\n", outfile = outfile)
}
fileConn <- file(file, "w")
vListConn <- file(vListFileName, "w")