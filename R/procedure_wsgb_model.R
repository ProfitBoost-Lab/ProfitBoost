#' @title
#' W-SGB
#'
#' @description
#' Procedure W-SGB Model
#'
#' @param myseed   seed for reproducibility of the results
#' @param mydata   input data
#' @param pos.covariates position (column number of the covariates)
#' @param delta    intervention cost
#' @param conditional whether the offer is conditional on renewal
#' @param loss     loss function
#' @param reorder  whether scores should be reordered (default: FALSE)
#' @param pi       individual expected profit lifts used as
#'                 DV and weights in the profit loss function - not needed for
#'                 classic loss increment, used to assess convergence in SGB
#'                 when computing optimal target size
#' @param pi.valid individual expected profit lifts used as
#'                 DV and weights in the profit loss function - not needed for
#'                 classic loss increment, used to assess convergence in SGB
#'                 when computing optimal target size
#' @param maxiter  maxiter hyperparameter SGB (default: 3000)
#' @param miniter  miniter hyperparameter SGB (default: 1500)
#' @param rho      rho hyperparameter SGB     (default: .001)
#' @param stoch    stoch hyperparameter SGB   (default: TRUE)
#' @param ratio    ratio hyperparameter SGB   (default: .3)
#' @param m0.train non-targeted margin scores reordered classic loss train sample
#' @param m1.train targeted margin scores reordered classic loss train sample
#' @param m0.valid non-targeted margin scores reordered classic loss valid sample
#' @param m1.valid targeted margin scores reordered classic loss valid sample
#' @param m0.test  non-targeted margin scores reordered classic loss test sample
#' @param m1.test  targeted margin scores reordered classic loss test sample
#' @param verbose  verbose output (default: TRUE)
#'
#' @return  List with W-SGB outcome measures.
#'
#' \itemize{
#'   \item myseed
#'   \item conditional
#'   \item delta
#'   \item train.data
#'   \item valid.data
#'   \item test.data
#'   \item scores.train
#'   \item scores.valid
#'   \item scores.test
#'   \item best.iteration.valid
#'   \item LogLik
#'   \item Variable.Importance
#'   \item maxlift.convergence.valid
#'   \item targetsize.convergence.valid
#' }
#'
#' @export
wsgb.procedure <- function(myseed,
                           mydata,
                           pos.covariates,
                           delta,
                           conditional,
                           loss,
                           reorder = FALSE,
                           pi,
                           pi.valid,
                           increment,
                           # used to assess convergence in SGB when
                           # computing optimal target size

                           # hyperparameters SGB
                           maxiter = 3000,
                           miniter = 1500,
                           rho = .001,
                           stoch = TRUE,
                           ratio = .3,

                           # to reorder scores for the reordered classic loss
                           m0.train,
                           m1.train,
                           m0.valid,
                           m1.valid,
                           m0.test,
                           m1.test,

                           verbose = TRUE) {

  # for reproducible results with published results
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(myseed)

  #---------------------------------------------------------------------------
  # split data in three random samples of equal sizes, not accounting for date
  #---------------------------------------------------------------------------
  ntrain <- floor(nrow(mydata)) / 3
  nvalid <- floor(nrow(mydata)) / 3
  ntest <- floor(nrow(mydata)) / 3
  mysamples <- create.3samples(
    mydata = mydata,
    size.train = ntrain,
    size.valid = nvalid,
    size.test = ntest,
    myseed = myseed
  )

  train.data <- mysamples$data.train
  valid.data <- mysamples$data.valid
  test.data <- mysamples$data.test

  #--------------------------------------
  # estimate Stochastic Gradient Boosting
  #--------------------------------------

  # when scores are re-ordered, set up following variables
  train.data.w0 <- train.data.w1 <- train.data
  train.data.w0$w <- 0
  train.data.w1$w <- 1
  valid.data.w0 <- valid.data.w1 <- valid.data
  valid.data.w0$w <- 0
  valid.data.w1$w <- 1
  test.data.w0 <- test.data.w1 <- test.data
  test.data.w0$w <- 0
  test.data.w1$w <- 1

  mymodel <- sgb.loss(
    data.train = train.data,
    data.valid = valid.data,
    data.test = test.data,
    otherdata1 = train.data.w0,
    otherdata2 = train.data.w1,
    # other data used for the reordered classic loss
    otherdata3 = valid.data.w0,
    otherdata4 = valid.data.w1,
    otherdata5 = test.data.w0,
    otherdata6 = test.data.w1,
    y.type = "zero/one",
    pos.y = pos.y,
    pos.covariates = pos.covariates,
    # adding w as covariate
    pi = pi,
    pi.valid = pi.valid,
    # individual expected profit lifts used as DV and weights in
    # the profit loss function - not needed for classic loss

    loss = loss,
    reorder = reorder,
    conditional = conditional,

    # hyperparameters SGB
    maxiter = maxiter,
    miniter = miniter,
    rho = rho,
    stoch = stoch,
    ratio = ratio,

    # info needed to determine convergence by calculating
    # optimal campaign profit at each iteration
    m.convergence.valid = valid.data$revenues,
    delta.convergence = delta,
    increment.convergence = increment,

    # to reorder scores for the reordered classic loss
    m0.train = m0.train,
    m1.train = m1.train,
    m0.valid = m0.valid,
    m1.valid = m1.valid,
    m0.test = m0.test,
    m1.test = m1.test,

    verbose = verbose
  )

  list(
    myseed = myseed,
    conditional = conditional,
    delta = delta,
    train.data = train.data,
    valid.data = valid.data,
    test.data = test.data,
    scores.train = mymodel$fmt,
    scores.valid = mymodel$fmv,
    scores.test = mymodel$fmts,
    best.iteration.valid = mymodel$best.iteration.valid,
    LogLik = mymodel$LogLik,
    Variable.Importance = mymodel$Variable.Importance,
    maxlift.convergence.valid = mymodel$maxlift.convergence.valid,
    targetsize.convergence.valid = mymodel$targetsize.convergence.valid
  )
}
