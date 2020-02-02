#===============#
# FUNCTION gini #
#===============#

# This function calculates the gini coefficient of a churn model, 
# given the estimated probabilities to churn.
# Inputs:
#   * y = a vector of length n containing the actual churn outcome of all customers
#   * p = a vector of length n containing the estimated probability or score to churn of all customers
# Outputs:
#   * gini = the gini coefficient

gini = function(y, p)
{
    helpdata =cbind(y, p)
    helpdata =helpdata[rev(order(p)),  ]
    n =dim(helpdata)[[1]]
    percy =cumsum(helpdata[, 1])/sum(y)
    perccust =seq(1, n)/n
    gini =2/n * (sum(percy - perccust))
    gini
}
