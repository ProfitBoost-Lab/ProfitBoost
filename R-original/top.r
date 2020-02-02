#==============#
# FUNCTION top #
#==============#

# This function calculates the top x% lift of a churn model, 
# given the estimated probabilities to churn.
# Inputs:
#   * y = a vector of length n containing the actual churn outcome of all customers
#   * p = a vector of length n containing the estimated probability or score to churn of all customers
#   * share = fraction (%) of customers to targeted, e.g. share = 0.1 is the top-decile
# Outputs:
#   * top = the top-x% lift

top=function(y, p, share)
{
    helpdata <- cbind(y, p)
    helpdata <- helpdata[rev(order(p)),  ]
    n <- dim(helpdata)[[1]]
    percy <- cumsum(helpdata[, 1])/sum(y)
    perccust <- seq(1, n)/n
    churnrate <- sum(y)/n
    index <- (perccust <= share)
    sample <- helpdata[index,  ]
    nsample <- dim(sample)[[1]]
    sumysample <- sum(sample[, 1])
    churnratesample <- sumysample/nsample
    topdecile <- churnratesample/churnrate
    topdecile
}
