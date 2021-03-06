# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Cosine Similarities
#'
#'
#' Given a User-Item Sparse Matrix, it will calculate a User-User Sparse Matrix of Cosine Similarities
#' 
#' @param X sp_mat 
cosine_similarity_mat <- function(X) {
    .Call('_RDataScience_cosine_similarity_mat', PACKAGE = 'RDataScience', X)
}

#' Cosine Similarities
#'
#'
#' Given a User-Item Sparse Matrix, it will calculate a User-User Sparse Matrix of Cosine Similarities
#' 
#' @param X sp_mat 
cosine_similarity <- function(X) {
    .Call('_RDataScience_cosine_similarity', PACKAGE = 'RDataScience', X)
}

#' Vector simple moving average
#'
#'
#' Returns a vector of the simple moving average of a vector
#' 
#' @param x Numeric vector
#' @param n lenght of the average 
sma <- function(x, n) {
    .Call('_RDataScience_sma', PACKAGE = 'RDataScience', x, n)
}

#' Simple moving average
#'
#'
#' Given a data table/frame with a column 'x', returns the simple moving average of that column
#' 
#' @param dt data table/frame
#' @param n lenght of the average 
simple_moving_avg <- function(dt, n) {
    .Call('_RDataScience_simple_moving_avg', PACKAGE = 'RDataScience', dt, n)
}

#' Weighted moving average
#'
#'
#' Given a data table/frame with a column 'x', returns the weighted moving average of that column
#' 
#' @param dt data table/frame
#' @param n lenght of the average 
weighted_moving_avg <- function(dt, n) {
    .Call('_RDataScience_weighted_moving_avg', PACKAGE = 'RDataScience', dt, n)
}

