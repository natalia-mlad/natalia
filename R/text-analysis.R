# library(psych)
# library(readxl)
# library(tidyverse)
# library(tidytext)
# library(stringr)
# library(tm)
# library(tokenizers)
# library(textreadr)
# library(quanteda)
# library(cleanNLP)
# library(udpipe)
#library(spacyr)

# Test:
# path = path_home(
#   "OneDrive/PhD Psychology/01 - Investigation - Developing Creditworthiness Measure/02 - Study 1b - Qualitative Interviews/05 - Transcription/"
# )
# txt = import.docx.text(path = path)
# create.nlp.tokens(txt = txt)

# 1. Importing ------------------------------------------------------------
#' import.docx.text
#'
#' @param path path
#' @export
import.docx.text <- function(path){
  filenames <- list.files(path = path, pattern = "*.docx", full.names = T)
  txt <- lapply(filenames, function(x) textreadr::read_docx(x))
  #map(filenames, ~textreadr::read_docx(.x))
  txt2 <- as.character(txt)
  #txt2 <- quanteda::corpus(txt2)
  return(txt2)
}


# 2. Pre-Processing -------------------------------------------------------
#' quick.preprocess
#'
#' @param txt txt
#'
#' @export
quick.preprocess <- function(txt) {
  require(qdap) #textclean
  # Pre-Processing:
  #txt2 <- tm_map(txt, tolower)
  txt2 <- replace_symbol(txt)
  txt2 <- replace_contraction(txt2) #*
  txt2 <- replace_number(txt2)
  txt2 <- replace_abbreviation(txt2)
  txt2 <- tolower(txt2)
  txt2 <- removePunctuation(txt2, ucp = T)
  #preserve_intra_word_dashes = T
  return(txt2)
}


# 3. Representing ---------------------------------------------------------
#' create.dtm
#'
#' @param txt txt
#'
#' @export
create.dtm <- function(txt) {
  require(tm)
  #VCorpus()
  #corpus <- Corpus(VectorSource(txt.char))
  corpus <- Corpus(VectorSource(txt))
  #summary(corpus)
  dtm <- DocumentTermMatrix(corpus, control = list(stopwords = T, stemming = T))
  return(dtm)
}

#' create.nlp.tokens
#'
#' @param txt txt
#' @param text_name text_name
#'
#' @export
#'
create.nlp.tokens <- function(txt, text_name = "texts") {
  #check txt is a corpus

  require(cleanNLP)
  #use_python("C:/Work/Programs/Anaconda3/python.exe", required=TRUE)
  #cnlp_download_corenlp(lang = "en")
  cnlp_init_udpipe()

  x <- cnlp_annotate(txt$documents, text_name = text_name)
  #x$token
  # A tibble: 639,233 x 11
  #x$document
  token <- x$token
  return(token)
}
##
#tid + tid_source
# relation <-
#   token %>%
#   group_by(relation) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# View(relation)
# token %>%
#   group_by(upos, relation) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
##
# token %>%
#   filter(upos == "PROPN") %>%
#   group_by(lemma) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# ##
# token %>%
#   filter(upos == "PROPN") %>%
#   group_by(token) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# token %>%
#   filter(upos == "NOUN") %>%
#   group_by(token) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))

pdf_text_extract <- function(path) {
  x <- pdftools::pdf_text(path)
  line_nums <- cumsum(lapply(tokenizers::tokenize_lines(x), length))
  if (any(line_nums == 0)) {
    warning("text not recognized in pdf")
    return(line_nums)
  }
  x_lines <- unlist(stringi::stri_split_lines(x))
  x_lines <- gsub("^\\s+|\\s+$", "", x_lines)
  x_lines <- pdfsearch:::remove_hyphen(x_lines)
  return(x_lines)
}
#
# pdf_text_extract(path) %>% as_tibble() %>%
# group_by(value) %>% summarise(n = n()) %>% arrange(desc(n))
##
# text <- paste(x_lines, collapse = " ")
# out <- list(line_nums = line_nums,
#             text_lines = x_lines,
#             full_text = text)
# return(out)
###
# #head(x_lines, 20) #length(x_lines) == 1
#x_lines <- unlist(stringi::stri_split_boundaries(x_lines, type = "sentence"))
###
# keyword = "history"
# keyword_line_loc <- lapply(seq_along(keyword), function(xx) grep(keyword[xx], x_lines, ignore.case = TRUE, perl = TRUE))
# (keyword_line <- unlist(keyword_line_loc))
