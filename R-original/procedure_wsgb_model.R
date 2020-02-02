#=======================#
# Procedure W-SGB Model #
#=======================#

wsgb.procedure = function(myseed, # seed for reproducibility of the results
                          mydata, # input data
                          pos.covariates, # position (column number of the covariates)
                          delta, # intervention cost
                          conditional, # whether the offer is conditional on renewal
                          loss, # loss function
                          reorder = FALSE, # whether scores should be reordred
                          pi, pi.valid, # individual expected profit lifts used as DV and weights in the profit loss function - not needed for classic loss
                          increment, # used to assess convergence in SGB when computing optimal target size
                          
                          # hyperparameters SGB
                          maxiter = 3000, 
                          miniter = 1500,
                          rho = .001,
                          stoch = TRUE,
                          ratio = .3,
                        
                          # to reorder scores for the reordered classic loss
                          m0.train, m1.train, 
                          m0.valid, m1.valid,
                          m0.test, m1.test,
                          
                          verbose = TRUE){

  set.seed(myseed)
  RNGkind(sample.kind = "Rounding" )  # for reproducible results with published results
  
  # split data in three random samples of equal sizes, not accounting for date
  #---------------------------------------------------------------------------
  ntrain = floor(nrow(mydata))/3
  nvalid = floor(nrow(mydata))/3
  ntest  = floor(nrow(mydata))/3
  mysamples = create.3samples(mydata = mydata,size.train = ntrain, size.valid = nvalid, size.test = ntest, myseed = myseed)

  train.data = mysamples$data.train
  valid.data = mysamples$data.valid
  test.data =  mysamples$data.test
  
  # estimate Stochastic Gradient Boosting
  #--------------------------------------
  
  # when scores are re-ordered, set up following variables
  train.data.w0 = train.data.w1 = train.data
  train.data.w0$w = 0
  train.data.w1$w = 1
  valid.data.w0 = valid.data.w1 = valid.data
  valid.data.w0$w = 0
  valid.data.w1$w = 1
  test.data.w0 = test.data.w1 = test.data
  test.data.w0$w = 0
  test.data.w1$w = 1
  
   mymodel = sgb.loss(data.train = train.data,data.valid=valid.data,data.test=test.data,
                          otherdata1 = train.data.w0,otherdata2=train.data.w1, # other data used for the reordered classic loss
                          otherdata3 = valid.data.w0,otherdata4=valid.data.w1,
                          otherdata5 = test.data.w0,otherdata6=test.data.w1,
                          y.type = "zero/one",pos.y=pos.y,pos.covariates=pos.covariates, #adding w as covariate
                          pi = pi, pi.valid = pi.valid, # individual expected profit lifts used as DV and weights in the profit loss function - not needed for classic loss
                          
                          loss = loss,reorder = reorder,conditional = conditional,

                          # hyperparameters SGB
                          maxiter=maxiter,miniter=miniter,
                          rho=rho,stoch=stoch,ratio=ratio,
                          
                          # info needed to determine convergence by calculating optimal campaign profit at each iteration
                          m.convergence.valid = valid.data$revenues,
                          delta.convergence = delta,
                          increment.convergence = increment,
                      
                          # to reorder scores for the reordered classic loss
                          m0.train=m0.train,m1.train=m1.train,
                          m0.valid=m0.valid,m1.valid=m1.valid,
                          m0.test=m0.test,m1.test=m1.test,
                      
                          verbose=verbose)  # add plots

    list(myseed = myseed,conditional = conditional, delta = delta,
     train.data = train.data, valid.data = valid.data, test.data = test.data,
     scores.train = mymodel$fmt,scores.valid = mymodel$fmv,scores.test = mymodel$fmts,# scores at best iteration
     best.iteration.valid = mymodel$best.iteration.valid,
     LogLik = mymodel$LogLik,
     Variable.Importance = mymodel$Variable.Importance,
     maxlift.convergence.valid = mymodel$maxlift.convergence.valid,
     targetsize.convergence.valid = mymodel$targetsize.convergence.valid)}
     
