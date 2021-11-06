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

# 1. Importing ------------------------------------------------------------
#' import.docx.text
#'
#' @param path path
#' @export
import.docx.text <- function(path){
  filenames <- list.files(path = path, pattern = "*.docx", full.names = T)
  txt <- lapply(filenames, function(x) read_docx(x))
  txt2 <- as.character(txt)
  txt2 <- corpus(txt2)
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
