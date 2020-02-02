# ================== #
# Verbeke's approach #
# ================== #

# formula : profit = N*alpha*((gamma*CLV+delta*(1-gamma))*beta0*lambda-delta-c)-A

# N = number of customers in the customer base
# alpha = proportion targeted (target size in %) --> let vary to find max profit
# beta0 = proportion of true would-be churners in customer base
# scores = predicted scores provided by the model
# delta = action cost = 12
# gamma = success rate of the incentive - > estimated as ATE
# c = cost of contact = 0
# CLV = average CLV
# A = fixed costs = 0

verbeke = function(y, scores,  m, delta,increment,gamma,  c = 0, A = 0, plot = TRUE) 
  {
  if (increment == "unit") target.sizes = 1:length(scores)
  if (increment != "unit") target.sizes = seq(0,length(scores),length(scores)*increment)[-1]
  
  campaign.profit.curve = c()
  for (i in target.sizes)
  {
    target.index = rev(order(scores))[1:i]  # indices of the targeted customers
    campaign.profit.curve = c(campaign.profit.curve,
                   length(target.index)*((gamma*mean(m[target.index],na.rm=TRUE)+delta*(1-gamma))*mean(y[target.index])-delta-c)-A)
  }
  # maximum reached by the campaign profit curve
  maxprofit = max(campaign.profit.curve,na.rm=TRUE)  # drop the missing due to empty target
  targetsize.maxprofit = min(target.sizes[maxprofit == campaign.profit.curve],na.rm=TRUE) # drop the missing and take the smallest sample needed

  if (plot == TRUE) 
  {plot(target.sizes,campaign.profit.curve,
        type="l",xlab = "Target Size", ylab = "Verbeke Profit")
    abline(h=maxprofit,lty=2)
    abline(v=targetsize.maxprofit,lty=2)}
  
  list(campaign.profit.curve=campaign.profit.curve,
       target.sizes=target.sizes,
       maxprofit=maxprofit,
       targetsize.maxprofit=targetsize.maxprofit,
       increment=increment)
  }
