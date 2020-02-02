#=============================#
# Main running script (MAIN)  #
#=============================#

source('create_3samples.r')
source('procedure_lift_model.r')
source('procedure_wsgb_model.r') 
source('SGBwithuserdefinedlossfunction.R')
source('campaignprofitlift.r')
source('targetsizeoptimization.r')
source('campaignevaluation.r')
source('fixedsizecampaignevaluation.r')
source('verbeke.r')
source('gini.r')
source('top.r')
source('overlapperdecile.r')

#-----------#
# load data #
#-----------#

# variable names:
# y = post-intervention churn 
# w = treated or not
# revenues = post-intervention cash flow 

load('mysynthdata.Rdata', verbose = T)

# data specification
colnumbers.covariates = 2:12  # indicates the position of all predictors excluding w (excludes w, y and revenues)
pos.covariates = 1:12 # indicates the position (column number) of all predictors including w (excludes w, y and revenues)
pos.y = 13  # indicates the position (column number) of y in the dataset
delta = 12 # intervention cost
conditional = TRUE # type of offer
increment = .02 # granularity level to determine optimal campaign size
budget = 1000 # budget to determine target size using budget constaint
buffer = .10 # add buffer of 10% to optimized target size

# hyperparameters for the SGB algoritm
maxiter = 3000
miniter = 1500
rho = .001 
stoch = TRUE
ratio = .3

B = 2 # change this number to get multiple iterations

holdoutprofit = array(NA,c(B,6)) # Holdout profit for all methods, including symmetric, left and right weighting (for Table 1)
fixedsize.churn = fixedsize.budget = optsize.verbeke = optsize.buffer = array(NA,c(B,1)) # Holdout profit for alternative target size selections (for Table 2)

holdoutgini = array(NA,c(B,4)) # Holdout gini for all methods (for Table 3)
holdouttdl = array(NA,c(B,4)) # Holdout tdl for all methods

TSlength = length(seq(0,nrow(mysynthdata)/3,nrow(mysynthdata)/3*increment)[-1])

holdoutcampaign.profit.curve.myliftmodel = 
  holdoutcampaign.profit.curve.mysgbmodel = holdoutcampaign.profit.curve.myrsgbmodel = 
  holdoutcampaign.profit.curve.mywsgbmodel = holdoutcampaign.profit.curve.mywsgb2model =
  holdoutcampaign.profit.curve.mywsgb3model = array(NA,c(B,TSlength)) # campaign profit curves

splitn=10
overlap.SGB.WSGB = overlap.RSGB.WSGB = overlap.LIFTrm.WSGB = array(NA,c(B,splitn+1)) # customer overlap curves for 10 deciles

RNGkind(sample.kind = "Rounding" )  # for reproducible results w.r.t. older versions of R

# start bootstrapping procedure:

for (b in 1:B){ print(b)

#-----------------------------------------#
# Uplift Models for Retention and Margins #
#-----------------------------------------#
print("Estimating Uplift Model")
# 1. estimate model on calibration sample, and make predictions for all observations
myliftproc = lift.procedure(myseed = b,
                            mydata = mysynthdata,
                            colnumbers.covariates = colnumbers.covariates, 
                            delta = delta,
                            conditional = conditional)

# 2. optimize target size in the validation sample
targetsize.mylift.valid = targetsizeoptimization(y = myliftproc$valid.data$y, 
                                                treated = myliftproc$valid.data$w, 
                                                scores = myliftproc$rmlift.valid,
                                                m = myliftproc$valid.data$revenues, 
                                                delta = delta,
                                                conditional = conditional,
                                                increment = increment, 
                                                plot = FALSE)$targetsize.maxprofit.trimmed/length(myliftproc$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.myliftmodel = campaignevaluation(y = myliftproc$test.data$y, 
                         treated = myliftproc$test.data$w, 
                         scores = myliftproc$rmlift.test,
                         m = myliftproc$test.data$revenues,
                         delta = delta,
                         conditional = conditional,
                         opttargetperc = targetsize.mylift.valid,
                         increment = increment, 
                         plot = TRUE)
holdoutprofit[b,1] = myeval.myliftmodel$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.myliftmodel[b,] = myeval.myliftmodel$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2

# 5. Calculate Holdout Gini Coefficient and Top Decile Lift
holdoutgini[b,1] = gini(y = myliftproc$test.data$y, p = myliftproc$rmlift.test)
holdouttdl[b,1] = top(y = myliftproc$test.data$y, p = myliftproc$rmlift.test, share = .1)
  
#-------------------#
# Classic SGB model #
#-------------------#
print("Estimating SGB with classic loss")
# 1. estimate w-sgb on calibration sample and make predictions on all three samples
mysgbproc =  wsgb.procedure(myseed = b,
                             mydata = mysynthdata,
                             pos.covariates = pos.covariates,
                             delta = delta,
                             conditional = conditional,
                             loss = "classic", # choice between symmetric weighting, left weighting, right weighting, equal weigthing
                             reorder = FALSE,
                             pi = (myliftproc$rmlift.train)/10, # divided by 10 to keep scale manageable (cf log computation)
                             pi.valid = (myliftproc$rmlift.valid)/10,
                             increment = increment,
                             maxiter = maxiter, miniter = miniter,
                             rho = rho, stoch = stoch, ratio = ratio,
                             m0.train = myliftproc$m0.train, # for re-ordered classic loss
                             m1.train = myliftproc$m0.train,
                             m0.valid = myliftproc$m0.valid,
                             m1.valid = myliftproc$m0.valid,
                             m0.test = myliftproc$m0.test,
                             m1.test = myliftproc$m0.test,
                             verbose = TRUE)

# 2. optimize target size in the validation sample
targetsize.sgb.valid = targetsizeoptimization(y = mysgbproc$valid.data$y, 
                                              treated = mysgbproc$valid.data$w, 
                                              scores = mysgbproc$scores.valid,
                                              m = mysgbproc$valid.data$revenues, 
                                              delta = delta,
                                              conditional = conditional,
                                              increment = increment, 
                                              plot = FALSE)$targetsize.maxprofit.trimmed/length(mysgbproc$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.mysgbmodel = campaignevaluation(y = mysgbproc$test.data$y, 
                                        treated = mysgbproc$test.data$w, 
                                        scores = mysgbproc$scores.test,
                                        m = mysgbproc$test.data$revenues,
                                        delta = delta,
                                        conditional = conditional,
                                        opttargetperc = targetsize.sgb.valid,
                                        increment = increment, 
                                        plot = TRUE)
holdoutprofit[b,2] = myeval.mysgbmodel$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.mysgbmodel[b,] = myeval.mysgbmodel$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2

# 5. Calculate Holdout Gini Coefficient and Top Decile Lift
holdoutgini[b,2] = gini(y = mysgbproc$test.data$y, p = mysgbproc$scores.test)
holdouttdl[b,2] = top(y = mysgbproc$test.data$y, p = mysgbproc$scores.test, share = .1)

#-----------------------------#
# Classic reordered SGB model #
#-----------------------------#
print("Estimating SGB with reordered classic loss")
# 1. estimate r-sgb on calibration sample and make predictions on all three samples
myrsgbproc =  wsgb.procedure(myseed = b,
                            mydata = mysynthdata,
                            pos.covariates = pos.covariates,
                            delta = delta,
                            conditional = conditional,
                            loss = "classic", # choice between symmetric weighting, left weighting, right weighting, equal weigthing
                            reorder = TRUE,
                            pi = (myliftproc$rmlift.train)/10, # divided by 10 to keep scale manageable (cf log computation)
                            pi.valid = (myliftproc$rmlift.valid)/10,
                            increment = increment,
                            maxiter = maxiter, miniter = miniter,
                            rho = rho, stoch = stoch, ratio = ratio,
                            m0.train = myliftproc$m0.train, # for re-ordered classic loss
                            m1.train = myliftproc$m0.train,
                            m0.valid = myliftproc$m0.valid,
                            m1.valid = myliftproc$m0.valid,
                            m0.test = myliftproc$m0.test,
                            m1.test = myliftproc$m0.test,
                            verbose = TRUE)

# 2. optimize target size in the validation sample
targetsize.rsgb.valid = targetsizeoptimization(y = myrsgbproc$valid.data$y, 
                                              treated = myrsgbproc$valid.data$w, 
                                              scores = myrsgbproc$scores.valid,
                                              m = myrsgbproc$valid.data$revenues, 
                                              delta = delta,
                                              conditional = conditional,
                                              increment = increment, 
                                              plot = FALSE)$targetsize.maxprofit.trimmed/length(myrsgbproc$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.myrsgbmodel = campaignevaluation(y = myrsgbproc$test.data$y, 
                                       treated = myrsgbproc$test.data$w, 
                                       scores = myrsgbproc$scores.test,
                                       m = myrsgbproc$test.data$revenues,
                                       delta = delta,
                                       conditional = conditional,
                                       opttargetperc = targetsize.rsgb.valid,
                                       increment = increment, 
                                       plot = TRUE)
holdoutprofit[b,3] = myeval.myrsgbmodel$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.myrsgbmodel[b,] = myeval.myrsgbmodel$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2

# 5. Calculate Holdout Gini Coefficient and Top Decile Lift
holdoutgini[b,3] = gini(y = myrsgbproc$test.data$y, p = myrsgbproc$scores.test)
holdouttdl[b,3] = top(y = myrsgbproc$test.data$y, p = myrsgbproc$scores.test, share = .1)

#-----------------------------------#
# Profit wSGB model, left weighting #
#-----------------------------------#
print("Estimating wSGB with left weighting")
# 1. estimate w-sgb on calibration sample and make predictions on all three samples
mywsgbproc =  wsgb.procedure(myseed = b,
                             mydata = mysynthdata,
                             pos.covariates = pos.covariates,
                             delta = delta,
                             conditional = conditional,
                             loss = "left weighting", # choice between symmetric weighting, left weighting, right weighting, equal weigthing
                             reorder = FALSE,
                             pi = (myliftproc$rmlift.train)/10, # divided by 10 to keep scale manageable (cf log computation)
                             pi.valid = (myliftproc$rmlift.valid)/10,
                             increment = increment,
                             maxiter = maxiter, miniter = miniter,
                             rho = rho, stoch = stoch, ratio = ratio,
                             m0.train = myliftproc$m0.train, # for re-ordered classic loss
                             m1.train = myliftproc$m0.train,
                             m0.valid = myliftproc$m0.valid,
                             m1.valid = myliftproc$m0.valid,
                             m0.test = myliftproc$m0.test,
                             m1.test = myliftproc$m0.test,
                             verbose = TRUE)

# 2. optimize target size in the validation sample
targetsize.wsgb.valid = targetsizeoptimization(y = mywsgbproc$valid.data$y, 
                                              treated = mywsgbproc$valid.data$w, 
                                              scores = mywsgbproc$scores.valid,
                                              m = mywsgbproc$valid.data$revenues, 
                                              delta = delta,
                                              conditional = conditional,
                                              increment = increment, 
                                              plot = FALSE)$targetsize.maxprofit.trimmed/length(mywsgbproc$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.mywsgbmodel = campaignevaluation(y = mywsgbproc$test.data$y, 
                                        treated = mywsgbproc$test.data$w, 
                                        scores = mywsgbproc$scores.test,
                                        m = mywsgbproc$test.data$revenues,
                                        delta = delta,
                                        conditional = conditional,
                                        opttargetperc = targetsize.wsgb.valid,
                                        increment = increment, 
                                        plot = TRUE)
holdoutprofit[b,4] = myeval.mywsgbmodel$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.mywsgbmodel[b,] = myeval.mywsgbmodel$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2

# 4. alternative target size selections

# 4.1. Fixed target size based on churnrate
fixedsize.churn[b] = fixedsizecampaignevaluation(y = mywsgbproc$test.data$y,
                                                   treated = mywsgbproc$test.data$w,  
                                                   scores = mywsgbproc$scores.test,
                                                   m = mywsgbproc$test.data$revenues,
                                                   delta = delta,
                                                   conditional = conditional,
                                                   fixedsizeperc = mean(mywsgbproc$valid.data$y), 
                                                   increment = increment)$profit.fixedsize  # holdout campaign profit at fixed size # TABLE 2

# 4.2. Fixed target size based on budget constraint
fixedsize.budget[b] = fixedsizecampaignevaluation(y = mywsgbproc$test.data$y,
                                                    treated = mywsgbproc$test.data$w,  
                                                    scores = mywsgbproc$scores.test,
                                                    m = mywsgbproc$test.data$revenues,
                                                    delta = delta,
                                                    conditional = conditional,
                                                    fixedsizeperc = (budget/delta)/length(mywsgbproc$test.data$y), 
                                                    increment = increment)$profit.fixedsize  # holdout campaign profit at fixed size # TABLE 2

# 4.3. Optimized target size based on Verbeke
targetsize.verbeke = verbeke(y = mywsgbproc$valid.data$y,
                             scores = mywsgbproc$scores.valid,
                             m = mywsgbproc$valid.data$revenues,
                             delta = delta, 
                             increment = increment,
                             gamma = mean(myliftproc$r0.valid-myliftproc$r1.valid), # i.e., average (estimated) churn lift
                             c = 0,  A = 0, plot = FALSE)$targetsize.maxprofit/length(mywsgbproc$valid.data$y)
optsize.verbeke[b] = campaignevaluation(y = mywsgbproc$test.data$y, 
                                          treated = mywsgbproc$test.data$w, 
                                          scores = mywsgbproc$scores.test,
                                          m = mywsgbproc$test.data$revenues,
                                          delta = delta,
                                          conditional = conditional,
                                          opttargetperc = targetsize.verbeke,
                                          increment = increment, 
                                          plot = TRUE)$campaign.evaluation

# 4.4. 10% Buffer
optsize.buffer[b] = campaignevaluation(y = mywsgbproc$test.data$y, 
                                         treated = mywsgbproc$test.data$w, 
                                         scores = mywsgbproc$scores.test,
                                         m = mywsgbproc$test.data$revenues,
                                         delta = delta,
                                         conditional = conditional,
                                         opttargetperc = targetsize.sgb.valid + buffer,
                                         increment = increment, 
                                         plot = TRUE)$campaign.evaluation

# 5. Calculate Holdout Gini Coefficient and Top Decile Lift
holdoutgini[b,4] = gini(y = mywsgbproc$test.data$y, p = mywsgbproc$scores.test)
holdouttdl[b,4] = top(y = mywsgbproc$test.data$y, p = mywsgbproc$scores.test, share = .1)

#------------------------#
# Overlap between models #
#------------------------#

overlap.SGB.WSGB[b,] = overlap.per.decile(mysgbproc$scores.test,mywsgbproc$scores.test,plot = F)
overlap.RSGB.WSGB[b,] = overlap.per.decile(myrsgbproc$scores.test,mywsgbproc$scores.test,plot = F)
overlap.LIFTrm.WSGB[b,] = overlap.per.decile(myliftproc$rmlift.test,mywsgbproc$scores.test,plot = F)


#-----------------------------------#
# Profit wSGB model, right weighting #
#-----------------------------------#
print("Estimating wSGB with right weighting")
# 1. estimate w-sgb on calibration sample and make predictions on all three samples
mywsgbproc2 =  wsgb.procedure(myseed = b,
                             mydata = mysynthdata,
                             pos.covariates = pos.covariates,
                             delta = delta,
                             conditional = conditional,
                             loss = "right weighting", # choice between symmetric weighting, left weighting, right weighting, equal weigthing
                             reorder = FALSE,
                             pi = (myliftproc$rmlift.train)/10, # divided by 10 to keep scale manageable (cf log computation)
                             pi.valid = (myliftproc$rmlift.valid)/10,
                             increment = increment,
                             maxiter = maxiter, miniter = miniter,
                             rho = rho, stoch = stoch, ratio = ratio,
                             m0.train = myliftproc$m0.train, # for re-ordered classic loss
                             m1.train = myliftproc$m0.train,
                             m0.valid = myliftproc$m0.valid,
                             m1.valid = myliftproc$m0.valid,
                             m0.test = myliftproc$m0.test,
                             m1.test = myliftproc$m0.test,
                             verbose = TRUE)

# 2. optimize target size in the validation sample
targetsize.wsgb2.valid = targetsizeoptimization(y = mywsgbproc2$valid.data$y, 
                                               treated = mywsgbproc2$valid.data$w, 
                                               scores = mywsgbproc2$scores.valid,
                                               m = mywsgbproc2$valid.data$revenues, 
                                               delta = delta,
                                               conditional = conditional,
                                               increment = increment, 
                                               plot = FALSE)$targetsize.maxprofit.trimmed/length(mywsgbproc2$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.mywsgb2model = campaignevaluation(y = mywsgbproc2$test.data$y, 
                                        treated = mywsgbproc2$test.data$w, 
                                        scores = mywsgbproc2$scores.test,
                                        m = mywsgbproc2$test.data$revenues,
                                        delta = delta,
                                        conditional = conditional,
                                        opttargetperc = targetsize.wsgb2.valid,
                                        increment = increment, 
                                        plot = TRUE)
holdoutprofit[b,5] = myeval.mywsgb2model$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.mywsgb2model[b,] = myeval.mywsgb2model$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2


#----------------------------------------#
# Profit wSGB model, symmetric weighting #
#----------------------------------------#
print("Estimating wSGB with symmetric weighting")
# 1. estimate w-sgb on calibration sample and make predictions on all three samples
mywsgbproc3 =  wsgb.procedure(myseed = b,
                              mydata = mysynthdata,
                              pos.covariates = pos.covariates,
                              delta = delta,
                              conditional = conditional,
                              loss = "symmetric weighting", # choice between symmetric weighting, left weighting, right weighting, equal weigthing
                              reorder = FALSE,
                              pi = (myliftproc$rmlift.train)/10, # divided by 10 to keep scale manageable (cf log computation)
                              pi.valid = (myliftproc$rmlift.valid)/10,
                              increment = increment,
                              maxiter = maxiter, miniter = miniter,
                              rho = rho, stoch = stoch, ratio = ratio,
                              m0.train = myliftproc$m0.train, # for re-ordered classic loss
                              m1.train = myliftproc$m0.train,
                              m0.valid = myliftproc$m0.valid,
                              m1.valid = myliftproc$m0.valid,
                              m0.test = myliftproc$m0.test,
                              m1.test = myliftproc$m0.test,
                              verbose = TRUE)

# 2. optimize target size in the validation sample
targetsize.wsgb3.valid = targetsizeoptimization(y = mywsgbproc3$valid.data$y, 
                                                treated = mywsgbproc3$valid.data$w, 
                                                scores = mywsgbproc3$scores.valid,
                                                m = mywsgbproc3$valid.data$revenues, 
                                                delta = delta,
                                                conditional = conditional,
                                                increment = increment, 
                                                plot = FALSE)$targetsize.maxprofit.trimmed/length(mywsgbproc3$valid.data$y) # optimal target size, in percentage

# 3. evaluate campaign profit on the holdout test sample
myeval.mywsgb3model = campaignevaluation(y = mywsgbproc3$test.data$y, 
                                         treated = mywsgbproc3$test.data$w, 
                                         scores = mywsgbproc3$scores.test,
                                         m = mywsgbproc3$test.data$revenues,
                                         delta = delta,
                                         conditional = conditional,
                                         opttargetperc = targetsize.wsgb3.valid,
                                         increment = increment, 
                                         plot = TRUE)
holdoutprofit[b,6] = myeval.mywsgb3model$campaign.evaluation # holdout campaign profit at optimized target size # TABLE 1
holdoutcampaign.profit.curve.mywsgb3model[b,] = myeval.mywsgb3model$campaign.profit.curve # campaign profit curve on the test sample # FIGURE 2

} # end of the boostrapping

save.image('results.Rdata')
