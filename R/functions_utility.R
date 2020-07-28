#' @title
#' Top
#'
#' @description
#' This function calculates the top x% lift of a churn model,
#' given the estimated probabilities to churn.
#'
#' @param y a vector of length n containing the actual churn outcome
#' of all customers
#' @param p a vector of length n containing the estimated probability or score
#' to churn of all customers
#' @param share fraction (%) of customers to targeted, e.g. share = 0.1
#' is the top-decile
#'
#' @return the top-x% lift
#'
#' @export
top <- function(y, p, share) {
  helpdata <- cbind(y, p)
  helpdata <- helpdata[rev(order(p)), ]
  n <- dim(helpdata)[[1]]
  percy <- cumsum(helpdata[, 1]) / sum(y)
  perccust <- seq(1, n) / n
  churnrate <- sum(y) / n
  index <- (perccust <= share)
  sample <- helpdata[index, ]
  nsample <- dim(sample)[[1]]
  sumysample <- sum(sample[, 1])
  churnratesample <- sumysample / nsample
  topdecile <- churnratesample / churnrate
  topdecile
}

#' @title
#' Gini
#'
#' @description
#' This function calculates the Gini coefficient of a churn model,
#' given the estimated probabilities to churn.
#'
#' @param y a vector of length n containing the actual churn outcome of
#' all customers
#' @param p a vector of length n containing the estimated probability or
#' score to churn of all customers
#'
#' @return the gini coefficient
#'
#' @export
gini <- function(y, p) {
  helpdata <- cbind(y, p)
  helpdata <- helpdata[rev(order(p)), ]
  n <- dim(helpdata)[[1]]
  percy <- cumsum(helpdata[, 1]) / sum(y)
  perccust <- seq(1, n) / n
  gini <- 2 / n * (sum(percy - perccust))
  gini
}
