#' @title
#' Customer overlap
#'
#' @description
#' Calculate customer overlap between two rankings
#'
#' @details
#' Overlap figure:
#'
#' 1. rank customers according to all methods' scores
#' 2. split in decile
#' 3. calculate per decile, the % customers overlapping in that decile
#'
#' See also Figure 4 in Lemmens, Aurélie, and Sunil Gupta.
#' "Managing churn to maximize profits." Marketing Science, (2020).
#'
#' @param score1 estimated score of first method for which to calculate overlap
#' @param score2 estimated score of second method for which to calculate overlap
#' @param splitn number of deciles (default: 10)
#' @param plot whether the overlap curve should be plotted (default: TRUE)
#'
#' @return Percentage Customers Overlapping.
#'
#' @export
overlap.per.decile <- function(score1,
                               score2,
                               splitn = 10,
                               plot = TRUE) {

  n       <- round(length(score1) / splitn)
  index1  <- rev(order(score1))
  index2  <- rev(order(score2))
  overlap <- 0

  for (decile in 1:splitn) {
    decile.index <- 1:(decile * n)
    overlap <- c(overlap,
                 length(intersect(index1[decile.index], index2[decile.index])) /
                          length(decile.index))
  }

  if (plot == TRUE) {
    graphics::plot(
      0:splitn,
      overlap * 100,
      type = "l",
      lty  = 1,
      xlab = "Decile",
      ylab = "Percentage Customers Overlapping",
      xaxp = c(0, 10, 10),
      yaxp = c(0, 100, 10)
    )
    lines(0:splitn, (0:splitn) * 10, lty = 2)
  }

  return(overlap)
}
