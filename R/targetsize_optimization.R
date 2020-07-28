#' @title
#' Target Size Optimization
#'
#' @description
#' Target Size Optimization in RCT.
#'
#' @param y post-intervention churn dummy
#' @param treated treatment dummy
#' @param scores  predictions
#' @param m post-intervention cash flows
#' @param delta intervention cost
#' @param conditional conditional offer or not (default: TRUE)
#' @param increment share of customers added at a time to build the campaign
#'                  profit curve (default: .1)
#' @param plot plot the campaign profit curve (default: TRUE)

#'
#' @return A list containing campaign profit curve, target sizes,
#' maximum profit and related variables.
#'
#' \itemize{
#'   \item campaign.profit.curve
#'   \item target.sizes
#'   \item maxprofit
#'   \item targetsize.maxprofit
#'   \item maxprofit.trimmed
#'   \item targetsize.maxprofit.trimmed
#'   \item secondbesttargetsize.trimmed
#'   \item increment
#' }
#'
#' @export
targetsizeoptimization <- function(y,
                                   treated,
                                   scores,
                                   m,
                                   delta,
                                   conditional = TRUE,
                                   increment   = .1,
                                   plot        = TRUE) {

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

  # maximum reached by the campaign profit curve
  maxprofit <- max(campaign.profit.curve, na.rm = TRUE)
  # drop the missing due to empty target

  # optimal target size, i.e., targeting that offers the highest campaign profit
  targetsize.maxprofit <- min(target.sizes[maxprofit == campaign.profit.curve],
    na.rm = TRUE
  )
  # drop the missing and take the smallest sample needed

  # also calculate maximum profit and optimal target size when removing the
  # smallest sample sizes (noisy)
  if (increment == "unit") {
    maxprofit.trimmed <- max(campaign.profit.curve[5:length(target.sizes)],
      na.rm = TRUE
    )

    secondbest.index <-
      rev(order(campaign.profit.curve[5:length(target.sizes)],
                na.last = FALSE))[2]

    secondbest.maxprofit.trimmed <-
      campaign.profit.curve[5:length(target.sizes)][secondbest.index]

    targetsize.maxprofit.trimmed <-
      min(target.sizes[5:length(target.sizes)]
          [maxprofit.trimmed == campaign.profit.curve[5:length(target.sizes)]],
          na.rm = TRUE
      )

    secondbest.targetsize.maxprofit.trimmed <-
      min(target.sizes[5:length(target.sizes)]
          [secondbest.maxprofit.trimmed ==
              campaign.profit.curve[5:length(target.sizes)]],
          na.rm = TRUE
      )
  }

  if (increment != "unit") {

    maxprofit.trimmed <-
      max(campaign.profit.curve[max((length(target.sizes) *
      increment), 5):length(target.sizes)], na.rm = TRUE)

    secondbest.index <-
      rev(order(campaign.profit.curve[max((length(target.sizes) *
      increment), 5):length(target.sizes)], na.last = FALSE))[2]

    secondbest.maxprofit.trimmed <-
      campaign.profit.curve[max((length(target.sizes) * increment), 5):
                              length(target.sizes)][secondbest.index]

    targetsize.maxprofit.trimmed <-
      min(target.sizes[max((length(target.sizes) * increment), 5):
                         length(target.sizes)]
          [maxprofit.trimmed == campaign.profit.curve[max(
            (length(target.sizes) * increment), 5):length(target.sizes)]],
          na.rm = TRUE)

    secondbest.targetsize.maxprofit.trimmed <-
      min(target.sizes[max((length(target.sizes) * increment), 5):
                         length(target.sizes)]
          [secondbest.maxprofit.trimmed ==
              campaign.profit.curve[max((length(target.sizes) *increment), 5):
                  length(target.sizes)]], na.rm = TRUE)
  }

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
    campaign.profit.curve        = campaign.profit.curve,
    target.sizes                 = target.sizes,
    maxprofit                    = maxprofit,
    targetsize.maxprofit         = targetsize.maxprofit,
    maxprofit.trimmed            = maxprofit.trimmed,
    targetsize.maxprofit.trimmed = targetsize.maxprofit.trimmed,
    secondbesttargetsize.trimmed = secondbest.targetsize.maxprofit.trimmed,
    increment                    = increment
  )
}
