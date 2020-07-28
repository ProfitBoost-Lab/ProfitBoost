#' @title
#' Lift Model Procedure
#'
#' @description
#' Procedure to Lift Models
#'
#' @param myseed seed to split the data set in 3 samples
#' @param mydata select the data to be split in 3 samples
#' @param colnumbers.covariates column numbers of predictors excluding w
#' @param delta cost of intervention
#' @param conditional whether the offer is conditional or not
#'
#' @return  List containing seed (myseed),
#'          whether the offer is conditional or not (conditional), and
#'          cost of intervention (delta).
#'          Followed by
#'          data (train.data, valid.data and test.data),
#'          probability of churn (non-targeted r0 /targeted r1),
#'          expected margin (non-targeted m0 /targeted m1),
#'          and lift (rmlift) for respectively
#'          train for train, valid, and test samples.
#'
#' \itemize{
#'   \item myseed
#'   \item conditional
#'   \item delta
#'   \item train.data
#'   \item valid.data
#'   \item test.data
#'   \item r0.train
#'   \item r0.valid
#'   \item r0.test
#'   \item r1.train
#'   \item r1.valid
#'   \item r1.test
#'   \item m0.train
#'   \item m0.valid
#'   \item m0.test
#'   \item m1.train
#'   \item m1.valid
#'   \item m1.test
#'   \item rmlift.train
#'   \item rmlift.valid
#'   \item rmlift.test
#' }
#'
#' @export
lift.procedure <- function(myseed,
                           mydata,
                           colnumbers.covariates,
                           delta,
                           conditional) {

  # for reproducible results with published results
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(myseed)

  #---------------------------------------------------------------------------
  # split data in three random samples of equal sizes, not accounting for date
  #---------------------------------------------------------------------------

  ntrain     <- floor(nrow(mydata)) / 3
  nvalid     <- floor(nrow(mydata)) / 3
  ntest      <- floor(nrow(mydata)) / 3

  mysamples  <- create.3samples(
    mydata     = mydata,
    size.train = ntrain,
    size.valid = nvalid,
    size.test  = ntest,
    myseed     = myseed
  )
  train.data <- mysamples$data.train
  valid.data <- mysamples$data.valid
  test.data  <- mysamples$data.test

  #------------------------------
  # estimate retention lift model
  #------------------------------

  myform      <- stats::as.formula(paste(
    "y ~ trt(w) + ",
    paste0(colnames(mydata[, colnumbers.covariates]),
      collapse   = "+"
    )
  ))

  myliftmodel <- upliftRF(
    myform,
    data         = train.data,
    ntree        = 100,
    split_method = "KL",
    verbose      = FALSE
  )

  pred.train  <- stats::predict(myliftmodel, train.data)
  pred.valid  <- stats::predict(myliftmodel, valid.data)
  pred.test   <- stats::predict(myliftmodel, test.data)

  # probability of churn if targeted on the train sample
  r1.train    <- pred.train[, 1]
  # probability of churn if targeted on the validation sample
  r1.valid    <- pred.valid[, 1]
  # probability of churn if targeted on the test sample
  r1.test     <- pred.test[, 1]

  # probability of churn if not targeted on the train sample
  r0.train    <- pred.train[, 2]
  # probability of churn if not targeted on the validation sample
  r0.valid    <- pred.valid[, 2]
  # probability of churn if not targeted on the test sample
  r0.test     <- pred.test[, 2]

  # estimate the cash flow lift model
  #----------------------------------
  mymodelvaluelift1 <- upliftKNN(
    train = train.data,
    test = train.data,
    y = train.data$revenues,
    ct = train.data$w,
    k = 10,
    dist.method = "euclidean",
    p = 2,
    ties.meth = "min",
    agg.method = "mean"
  )

  mymodelvaluelift2 <- upliftKNN(
    train = train.data,
    test = valid.data,
    y = train.data$revenues,
    ct = train.data$w,
    k = 10,
    dist.method = "euclidean",
    p = 2,
    ties.meth = "min",
    agg.method = "mean"
  )

  mymodelvaluelift3 <- upliftKNN(
    train = train.data,
    test = test.data,
    y = train.data$revenues,
    ct = train.data$w,
    k = 10,
    dist.method = "euclidean",
    p = 2,
    ties.meth = "min",
    agg.method = "mean"
  )

  # expected margin if targeted on the train sample,
  # ! col is switched compared to retention model above
  m1.train <- mymodelvaluelift1[, 2]
  # expected margin if targeted on the validation sample
  m1.valid <- mymodelvaluelift2[, 2]
  # expected margin if targeted on the test sample
  m1.test <- mymodelvaluelift3[, 2]

  # expected margin if not targeted on the train sample
  m0.train <- mymodelvaluelift1[, 1]
  # expected margin if not targeted on the validation sample
  m0.valid <- mymodelvaluelift2[, 1]
  # expected margin if not targeted on the test sample
  m0.test <- mymodelvaluelift3[, 1]

  # conditional lift
  if (conditional == TRUE) {
    rmlift.train <- ((1 - r1.train) * (m1.train - delta)) - ((1 - r0.train) *
      m0.train)
    rmlift.valid <- ((1 - r1.valid) * (m1.valid - delta)) - ((1 - r0.valid) *
      m0.valid)
    rmlift.test <- ((1 - r1.test) * (m1.test - delta)) - ((1 - r0.test) *
      m0.test)
  }

  # unconditional lift
  if (conditional == FALSE) {
    rmlift.train <- ((1 - r1.train) * m1.train) -
      ((1 - r0.train) * m0.train) -
      delta
    rmlift.valid <- ((1 - r1.valid) * m1.valid) - ((1 - r0.valid) * m0.valid) -
      delta
    rmlift.test <- ((1 - r1.test) * m1.test) - ((1 - r0.test) * m0.test) -
      delta
  }

  list(
    myseed       = myseed,
    conditional  = conditional,
    delta        = delta,
    train.data   = train.data,
    valid.data   = valid.data,
    test.data    = test.data,
    r0.train     = r0.train,
    r0.valid     = r0.valid,
    r0.test      = r0.test,
    r1.train     = r1.train,
    r1.valid     = r1.valid,
    r1.test      = r1.test,
    m0.train     = m0.train,
    m0.valid     = m0.valid,
    m0.test      = m0.test,
    m1.train     = m1.train,
    m1.valid     = m1.valid,
    m1.test      = m1.test,
    rmlift.train = rmlift.train,
    rmlift.valid = rmlift.valid,
    rmlift.test  = rmlift.test
  )
}
