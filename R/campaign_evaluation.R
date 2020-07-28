#' @title
#' SGB with user-defined loss function
#'
#' @description
#' SGB with user-defined loss function
#'
#' @details
#' Calculate the (profit) lift for a given target size
#' based on the estimated scores
#'
#' @param y post-intervention churn
#' @param treated whether the customer is treated
#' @param scores estimated scores
#' @param m post-intervention cash flow
#' @param delta intervention cost
#' @param conditional whether the offer is conditional on renewal
#' @param opttargetperc vector of length k of given target size for each method
#' @param increment share of customers added to target size, if "unit", customers are added one by one
#' @param plot whether the campaign profit curve should be plotted
#'
#' @return A list containing holdout campaign profit at optimal target size
#'         (as determined from the validation sample), campaign profit curve
#'          on the test sample, range of target sizes, and
#'          the optimal target size determined on the validation sample.
#'
#' \itemize{
#'   \item campaign.evaluation
#'   \item campaign.profit.curve
#'   \item target.sizes
#'   \item targetsize.maxprofit
#' }
#'
#' @export
campaignevaluation <- function(y,
                               treated,
                               scores,
                               m,
                               delta,
                               conditional = TRUE,
                               opttargetperc,
                               increment = .1,
                               plot) {
  scores <- as.matrix(scores)

  # campaign evaluation at provided target size opttargetperc
  ce <- array(NA, c(ncol(scores), 1))

  # percentage * the number of observations = number
  targetsize.maxprofit <- opttargetperc * nrow(scores)

  for (k in 1:ncol(scores)) {
    target.index <- rev(order(scores[, k]))[1:targetsize.maxprofit[k]] # indices of the targeted customers
    ce[k] <- campaignprofitlift(y[target.index], treated[target.index],
      m[target.index], delta,
      conditional = conditional
    )
  }

  if (increment == "unit") {
    target.sizes <- 1:length(scores)
  }
  if (increment != "unit") {
    target.sizes <- seq(0, length(scores), length(scores) * increment)[-1]
  }

  campaign.profit.curve <- c()
  for (i in target.sizes)
  {
    target.index <- rev(order(scores))[1:i] # indices of the targeted customers
    campaign.profit.curve <- c(
      campaign.profit.curve,
      campaignprofitlift(y[target.index], treated[target.index],
        m[target.index], delta,
        conditional = conditional
      )
    )
  }


  print(target.sizes)
  print(campaign.profit.curve)

  # plot the campaign profit curve as a function of the target size
  if (plot == TRUE) {
    graphics::plot(
      target.sizes,
      campaign.profit.curve,
      type = "l",
      xlab = "Target Size",
      ylab = "Campaign Profitability"
    )
    graphics::abline(v = targetsize.maxprofit, lty = 2)
  }

  list(
    campaign.evaluation = ce,
    campaign.profit.curve = campaign.profit.curve,
    target.sizes = target.sizes,
    targetsize.maxprofit = targetsize.maxprofit
  )
}
