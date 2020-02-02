#=======================#
# Procedure Lift Models #
#=======================#

lift.procedure = function(myseed, # seed to split the dataset in 3 samples   
                          mydata, # select the data to be split in 3 samples
                          colnumbers.covariates, # column numbers of predictors excluding w
                          delta, # cost of intervention
                          conditional){ # whether the offer is conditional or not

  library(uplift)
  set.seed(myseed)
  RNGkind(sample.kind = "Rounding" )  # for reproducible results with published results
  
  # split data in three random samples of equal sizes, not accounting for date
  #---------------------------------------------------------------------------
  ntrain = floor(nrow(mydata))/3
  nvalid = floor(nrow(mydata))/3
  ntest  = floor(nrow(mydata))/3
  mysamples = create.3samples(mydata = mydata, size.train = ntrain, size.valid = nvalid, size.test = ntest, myseed = myseed)
  train.data = mysamples$data.train
  valid.data = mysamples$data.valid
  test.data =  mysamples$data.test
  
  # estimate retention lift model
  #------------------------------
  myform=as.formula(paste("y ~ trt(w) + ",paste0(colnames(mydata[,colnumbers.covariates]), collapse='+')))
  myliftmodel <- upliftRF(myform,
                            data = train.data,
                            ntree = 100,
                            split_method = "KL",
                            verbose = FALSE)
    
  pred.train = predict(myliftmodel, train.data)
  pred.valid = predict(myliftmodel, valid.data)
  pred.test = predict(myliftmodel, test.data)
    
  r1.train = pred.train[,1] #proba of churn if targeted on the train sample
  r1.valid = pred.valid[,1] #proba of churn if targeted on the validation sample
  r1.test = pred.test[,1]   #proba of churn if targeted on the test sample
    
  r0.train = pred.train[,2] #proba of churn if not targeted on the train sample
  r0.valid = pred.valid[,2] #proba of churn if not targeted on the validation sample
  r0.test = pred.test[,2]   #proba of churn if not targeted on the test sample
   
  # estimate the cash flow lift model
  #----------------------------------  
  mymodelvaluelift1 = upliftKNN(train=train.data, 
                                  test=train.data,
                                  y=train.data$revenues, 
                                  ct=train.data$w, k = 10, 
                                  dist.method = "euclidean", p = 2, ties.meth = "min",   agg.method = "mean")
    
  mymodelvaluelift2 = upliftKNN(train=train.data, 
                                  test=valid.data,
                                  y=train.data$revenues,
                                  ct=train.data$w, k = 10, 
                                  dist.method = "euclidean", p = 2, ties.meth = "min",   agg.method = "mean")
    
  mymodelvaluelift3 = upliftKNN(train=train.data, 
                                  test=test.data,
                                  y=train.data$revenues, 
                                  ct=train.data$w, k = 10, 
                                  dist.method = "euclidean", p = 2, ties.meth = "min",   agg.method = "mean")
    
  m1.train = mymodelvaluelift1[,2]  # expected margin if targeted on the train sample, ! col is switched compared to retention model above
  m1.valid = mymodelvaluelift2[,2]  # expected margin if targeted on the validation sample
  m1.test = mymodelvaluelift3[,2]   # expected margin if targeted on the test sample
    
  m0.train = mymodelvaluelift1[,1]  # expected margin if not targeted on the train sample
  m0.valid = mymodelvaluelift2[,1]  # expected margin if not targeted on the validation sample
  m0.test = mymodelvaluelift3[,1]   # expected margin if not targeted on the test sample
    
  # conditional gift
  if (conditional == TRUE)
    {
      rmlift.train = ((1-r1.train)*(m1.train-delta))-((1-r0.train)*m0.train) 
      rmlift.valid = ((1-r1.valid)*(m1.valid-delta))-((1-r0.valid)*m0.valid) 
      rmlift.test = ((1-r1.test)*(m1.test-delta))-((1-r0.test)*m0.test) 
    }
    
  # unconditional gift
  if (conditional == FALSE)
    {
      rmlift.train = ((1-r1.train)*m1.train)-((1-r0.train)*m0.train)-delta
      rmlift.valid = ((1-r1.valid)*m1.valid)-((1-r0.valid)*m0.valid) -delta
      rmlift.test = ((1-r1.test)*m1.test)-((1-r0.test)*m0.test) -delta
    }
    
list(myseed = myseed,conditional = conditional, delta = delta,
     train.data = train.data, valid.data = valid.data, test.data = test.data,
     r0.train = r0.train, r0.valid = r0.valid, r0.test = r0.test,
     r1.train = r1.train, r1.valid = r1.valid, r1.test = r1.test,
     m0.train = m0.train, m0.valid = m0.valid, m0.test = m0.test,
     m1.train = m1.train, m1.valid = m1.valid, m1.test = m1.test,
     rmlift.train = rmlift.train, rmlift.valid = rmlift.valid, rmlift.test = rmlift.test)
}
