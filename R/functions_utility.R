#' @title
#' Verbeke's approach
#'
#' @description
#' Verbeke's approach to maximize profit margin.
#'
#' @details
#' Formula:
#'
#' profit = N*alpha*((gamma*CLV+delta*(1-gamma))*beta0*lambda-delta-c)-A
#'
#' Where:
#'
#' N = number of customers in the customer base
#' alpha = proportion targeted (target size in %) - vary to find max profit
#' beta0 = proportion of true would-be churners in customer base
#' scores = predicted scores provided by the model
#' delta = action cost = 12
#' gamma = success rate of the incentive - > estimated as ATE
#' c = cost of contact = 0
#' CLV = average customer lifetime value
#' A = fixed costs = 0
#'
#' @param N Number of customers in the customer base
#' @param alpha proportion targeted (target size in %) - vary to find max profit
#' @param beta0 proportion of true would-be churners in customer base
#' @param scores predicted scores provided by the model
#' @param delta action cost [default: 12]
#' @param gamma success rate of the incentive (estimated as ATE)
#' @param c cost of contact [default: 0]
#' @param A Fixed costs [default: 0]
#' @param Plot Plot the results? [default: false]
#'
#' @return A list containing campaign profit curve, target sizes,
#' maximum profit and related variables.
#'
#' \itemize{
#'   \item campaign.profit.curve
#'   \item target.sizes
#'   \item maxprofit
#'   \item targetsize.maxprofit
#'   \item increment
#' }
#'
#' @export
verbeke = function(y,
                   scores,
                   m,
                   delta,
                   increment,
                   gamma,
                   c = 0,
                   A = 0,
                   plot = TRUE)
{
  if (increment == "unit") {
    target.sizes = 1:length(scores)
  }

  if (increment != "unit") {
    target.sizes = seq(0, length(scores), length(scores) * increment)[-1]
  }

  campaign.profit.curve = c()
  for (i in target.sizes)
  {
    # indices of the targeted customers
    target.index = rev(order(scores))[1:i]
    campaign.profit.curve = c(campaign.profit.curve,
                              length(target.index) * ((
                                gamma * mean(m[target.index], na.rm = TRUE) +
                                  delta * (1 - gamma)
                              ) * mean(y[target.index]) - delta - c) - A)
  }

  # maximum reached by the campaign profit curve
  # drop the missing due to empty target
  maxprofit = max(campaign.profit.curve, na.rm = TRUE)

  # drop the missing and take the smallest sample needed
  targetsize.maxprofit = min(target.sizes[maxprofit == campaign.profit.curve],
                             na.rm = TRUE)

  if (plot == TRUE)
  {
    graphics::plot(
      target.sizes,
      campaign.profit.curve,
      type = "l",
      xlab = "Target Size",
      ylab = "Verbeke Profit"
    )
    graphics::abline(h = maxprofit, lty = 2)
    graphics::abline(v = targetsize.maxprofit, lty = 2)
  }

  list(
    campaign.profit.curve = campaign.profit.curve,
    target.sizes = target.sizes,
    maxprofit = maxprofit,
    targetsize.maxprofit = targetsize.maxprofit,
    increment = increment
  )
}


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
top = function(y, p, share)
{
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
#' This function calculates the gini coefficient of a churn model,
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
gini = function(y, p)
{
  helpdata = cbind(y, p)
  helpdata = helpdata[rev(order(p)), ]
  n = dim(helpdata)[[1]]
  percy = cumsum(helpdata[, 1]) / sum(y)
  perccust = seq(1, n) / n
  gini = 2 / n * (sum(percy - perccust))
  gini
}
