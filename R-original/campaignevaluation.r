#=====================================================#
# Calculate the (profit) lift for a given target size #
# based on the estimated scores                       #
#=====================================================#

# y: post-intervention churn
# treated: whether the customer is treated
# scores: estimated scores
# m: post-intervention cash flow
# delta: intervention cost
# conditional: whether the offer is conditional on renewal
# opttargetperc: vector of length k of given target size for each method
# increment: share of customers added to target size, if "unit", customers are added one by one
# plot: whether the campaign profit curve should be plotted

campaignevaluation = function(y, treated, scores, m, delta,
                              conditional = TRUE,
                              opttargetperc,
                              increment = .1, plot)
{ 
  scores = as.matrix(scores)
  ce = array(NA,c(ncol(scores),1))  # campaign evaluation at provided target size opttargetperc
  targetsize.maxprofit = opttargetperc*nrow(scores) # percentage * the number of observations = number
  
  for (k in 1:ncol(scores)) {
      target.index = rev(order(scores[,k]))[1:targetsize.maxprofit[k]]  # indices of the targeted customers
      ce[k] = campaignprofitlift(y[target.index], treated[target.index],
                            m[target.index], delta,conditional = conditional)}
  
  if (increment == "unit") target.sizes = 1:length(scores)
  if (increment != "unit") target.sizes = seq(0,length(scores),length(scores)*increment)[-1]
  
  campaign.profit.curve = c()
  for (i in target.sizes)
  {
    target.index = rev(order(scores))[1:i]  # indices of the targeted customers
    campaign.profit.curve = c(campaign.profit.curve,
                              campaignprofitlift(y[target.index], treated[target.index],
                                                 m[target.index], delta, conditional = conditional))
  }
  
  # plot the campaign profit curve as a function of the target size
  if (plot == TRUE) 
  {plot(target.sizes,campaign.profit.curve,
        type="l",xlab = "Target Size", ylab = "Campaign Profitability")
    abline(v=targetsize.maxprofit,lty=2)}
    
  list(campaign.evaluation = ce, # holdout campaign profit at optimal target size (as determined from the validation sample)
       campaign.profit.curve = campaign.profit.curve, # campaign profit curve on the test sample
       target.sizes=target.sizes, # range of target sizes
       targetsize.maxprofit = targetsize.maxprofit) # optimal target size determined on the validation sample
}