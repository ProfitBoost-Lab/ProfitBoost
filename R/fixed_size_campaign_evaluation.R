#' @title
#' Fixed target size determination
#'
#' @description
#' Specifies the target size exogenously and calculate the corresponding
#' campaign profit.
#'
#' @param y post-intervention churn
#' @param treated whether the customer is treated
#' @param scores estimated scores
#' @param m post-intervention cash flow
#' @param delta intervention cost
#' @param conditional whether the offer is conditional on renewal (default:TRUE)
#' @param fixedsizeperc target size in percentage customers being targeted
#' @param increment share of customers added to target size, if "unit",
#'                  customers are added one by one
#'
#' @return A list containing campaign profit and target size
#'
#' \itemize{
#'   \item profit.fixedsize
#'   \item targetsize.fixedsize
#' }
#'
#' @export
fixedsizecampaignevaluation <- function(y,
                                        treated,
                                        scores,
                                        m,
                                        delta,
                                        conditional = TRUE,
                                        fixedsizeperc,
                                        increment = .1) {
  if (increment == "unit") {
    target.sizes <- seq_len(length(scores))
  }

  if (increment != "unit") {
    target.sizes <- seq(0, length(scores), length(scores) * increment)[-1]
  }

  fixedsize <- fixedsizeperc * length(scores)

  # takes into account increment
  fixedsizeposition <- order(abs(target.sizes - fixedsize))[1]

  campaign.profit.curve <- c()
  for (i in target.sizes) {
    # indices of the targeted customers
    target.index <- rev(order(scores))[1:i]
    campaign.profit.curve <- c(
      campaign.profit.curve,
      campaignprofitlift(y[target.index], treated[target.index],
        conditional = conditional,
        m[target.index], delta
      )
    )
  }
  profit.fixedsize <- campaign.profit.curve[fixedsizeposition]
  targetsize.fixedsize <- target.sizes[fixedsizeposition]

  list(
    profit.fixedsize = profit.fixedsize,
    targetsize.fixedsize = targetsize.fixedsize
  )
}
