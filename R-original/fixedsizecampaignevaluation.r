#=================================#
# Fixed target size determination #
#=================================#

# specifies the target size exogeneously and calculate the corresponding campaign profit

# y: post-intervention churn
# treated: whether the customer is treated
# scores: estimated scores
# m: post-intervention cash flow
# delta: intervention cost
# conditional: whether the offer is conditional on renewal
# fixedsizeperc: target size in percentage customers being targeted
# increment: share of customers added to target size, if "unit", customers are added one by one

fixedsizecampaignevaluation = function(y,
                                       treated,
                                       scores,
                                       m,
                                       delta,
                                       conditional = TRUE,
                                       fixedsizeperc,
                                       increment = .1)
{
  if (increment == "unit")
    target.sizes = 1:length(scores)
  if (increment != "unit")
    target.sizes = seq(0, length(scores), length(scores) * increment)[-1]

  fixedsize = fixedsizeperc * length(scores)
  fixedsizeposition = order(abs(target.sizes - fixedsize))[1] # takes into account increment

  campaign.profit.curve = c()
  for (i in target.sizes)
  {
    target.index = rev(order(scores))[1:i]  # indices of the targeted customers
    campaign.profit.curve = c(
      campaign.profit.curve,
      campaignprofitlift(y[target.index], treated[target.index], conditional = conditional,
                         m[target.index], delta)
    )
  }
  profit.fixedsize = campaign.profit.curve[fixedsizeposition]
  targetsize.fixedsize = target.sizes[fixedsizeposition]

  list(profit.fixedsize = profit.fixedsize,
       # campaign profit
       targetsize.fixedsize = targetsize.fixedsize) # target size
}
