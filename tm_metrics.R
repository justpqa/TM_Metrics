library(tidyverse)
library(tidyr)
library(R.utils)
library(dplyr)
library(quanteda)
require(quanteda.textstats)
require(readtext)
library(stm)
library(tm)
library(topicmodels)
library(stringr)
library(parallel)
library(Rcpp)
sourceCpp("/home/rstudio/users/roback/PoliticalRhetoric/.Ruserdata/phan4/helper.cpp") # will be used for the topic coherence metrics

# using CV coherence for describing how the top word from each topic would go with each other
dieng_topic_coherence <- function(stm_model, doc_vec, N = 10) {

  # First we need to calculate the contribution of 1 topic to the final term
  topic_dieng_contrib <- function(doc_vec, top_words) {
    # store current timstamp

    # need to store related variable
    M <- length(doc_vec)
    N <- length(top_words)

    helper <- function(doc) {
      return(word_in_doc(doc, top_words))
    }

    # should return a matrix where each row is a bw and an indicator of having a certain word in it
    # sometimes does not simplify if the output have different size (only convert to a matrix if has similar number of items)
    # so must have SIMPLIFY = FALSE and the convert to a matrix later
    count_mat <- mapply(helper, doc_vec, SIMPLIFY = FALSE)
    count_mat <- do.call(rbind, count_mat) # convert a list of matrix with similar number of columns to a big matrix

    # now we calculate npmi score
    return(count_matrix_to_dieng_contrib(count_mat))
  }

  top_words_mat <- labelTopics(stm_model, n = N)$frex

  total <- 0
  # iterate through each row (each topic), and calculate its contribution to final sum
  for (i in 1:stm_model$settings$dim$K) {
    temp <- topic_dieng_contrib(doc_vec, top_words_mat[i, ])
    total <- total + temp
  }
  return((2*total) / (N * (N-1) * stm_model$settings$dim$K))
}

cv_topic_coherence <- function(stm_model, doc_vec, N = 10, window = 110) {

  # First we need to calculate the contribution of 1 topic to the final term
  topic_cv_contrib <- function(doc_vec, top_words, window) {
    # store current timstamp

    # need to store related variable
    M <- length(doc_vec)
    N <- length(top_words)

    helper <- function(doc) {
      return(count_window_has_word(doc, top_words, window))
    }

    # should return a matrix where each row is a bw and an indicator of having a certain word in it
    # sometimes does not simplify if the output have different size (only convert to a matrix if has similar number of items)
    # so must have SIMPLIFY = FALSE and the convert to a matrix later
    count_mat <- mapply(helper, doc_vec, SIMPLIFY = FALSE)
    count_mat <- do.call(rbind, count_mat) # convert a list of matrix with similar number of columns to a big matrix

    # now we calculate npmi score
    return(count_matrix_to_cv_contrib(count_mat))
  }

  # try to use frex instead of prob
  top_words_mat <- labelTopics(stm_model, n = N)$frex

  total <- 0
  # iterate through each row (each topic), and calculate its contribution to final sum
  for (i in 1:stm_model$settings$dim$K) {
    temp <- topic_cv_contrib(doc_vec, top_words_mat[i, ], window)
    total <- total + temp
  }
  return(total / (N * stm_model$settings$dim$K))
}

# topic diversity: proportion of unique words in the top words of all topics
topic_diversity <- function(stm_model, N = 5) {
  top_words_mat <- labelTopics(stm_model, n = N)$prob
  # return # unique words / total number of words
  return(nrow(as.data.frame(table(top_words_mat))) / (N * stm_model$settings$dim$K))
}