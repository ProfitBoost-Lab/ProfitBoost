#===============================#
# Calculate the Campaign Profit #
#===============================#

campaignprofitlift = function(y, 
                              treated,
                              m, 
                              delta, 
                              conditional = TRUE) # returns NA when one group is empty
{
  if (conditional == TRUE) {return(length(y)*((sum(m[y==0 & treated == 1]-delta)/sum(treated ==1))-
                                                (sum(m[y==0 & treated == 0])/sum(treated ==0))))}
  if (conditional == FALSE) {return(length(y)*((sum(m[y==0 & treated == 1])/sum(treated ==1))-
                                                 (sum(m[y==0 & treated == 0])/sum(treated ==0))-delta))}
}
