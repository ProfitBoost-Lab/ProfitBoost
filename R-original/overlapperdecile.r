#=================================================#
# Calculate customer overlap between two rankings #
#=================================================#

# Figure 4 in the paper

# Overlap figure:
# 1. rank customers according to all methods' scores
# 2. split in decile
# 3. calculate per decile, the % customers overlapping in that decile
#====================================================================

# score1 and score2 = the estimated scores for both methods for which overlap should be calculated
# splitn = number of deciles (e.g. 10)
# plot = whether the overlap curve should be plotted

overlap.per.decile = function(score1,score2,splitn=10,plot=TRUE) {
  n = round(length(score1)/splitn)
  index1 = rev(order(score1))
  index2 = rev(order(score2))
  overlap = 0
  for (decile in 1:splitn) {
    decile.index = 1:(decile*n)
    overlap = c(overlap,length(intersect(index1[decile.index],index2[decile.index]))/length(decile.index))}
  if (plot==TRUE){
  plot(0:splitn,overlap*100,type="l",lty=1,xlab="Decile",
       ylab="Percentage Customers Overlapping",xaxp=c(0,10,10),yaxp=c(0,100,10))
  lines(0:splitn,(0:splitn)*10,lty=2)}
  return(overlap)
}
