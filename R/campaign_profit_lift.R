#' @title
#' Campaign profit lift
#'
#' @description
#' Calculate the Campaign Profit Lift - returns NA when one group is empty.
#'
#' @param y post-intervention churn of targeted customers
#' @param treated whether the customer is treated
#' @param m post-intervention cash flow
#' @param delta intervention cost
#' @param conditional whether the offer is conditional
#'                    on renewal (default: TRUE)
#'
#' @return campaign profit
#'
#' @export
campaignprofitlift <- function(y,
                               treated,
                               m,
                               delta,
                               conditional = TRUE) {

  if (conditional == TRUE) {
    return(length(y) *
             ((sum(m[y == 0 & treated == 1] - delta) / sum(treated == 1)) -
                (sum(m[y == 0 & treated == 0]) / sum(treated == 0))))
  }

  if (conditional == FALSE) {
    return(length(y) *
             ((sum(m[y == 0 & treated == 1]) / sum(treated == 1)) -
                (sum(m[y == 0 & treated == 0]) / sum(treated == 0)) - delta))
  }
}
