#' @title
#' SGB with user-defined loss function
#'
#' @description
#' SGB with user-defined loss function.
#'
#' @details
#' This function applies the SGB algorithm with a user-defined loss function.
#' It calibrates the model on a calibration sample and generates predictions
#' on a validation and test samples.
#'
#' @param data.train training dataset
#' @param data.valid validation dataset
#' @param data.test  test dataset
#' @param otherdata1 other data used for the reordered classic loss, 1
#' @param otherdata2 other data used for the reordered classic loss, 2
#' @param otherdata3 other data used for the reordered classic loss, 3
#' @param otherdata4 other data used for the reordered classic loss, 4
#' @param otherdata5 other data used for the reordered classic loss, 5
#' @param otherdata6 other data used for the reordered classic loss, 6
#' @param y.type churn variable,  and
#' @param pos.y positions (col) of churn
#' @param pos.covariates covariates
#' @param pi        individual expected profit lifts used as DV and weights
#'                  in the profit loss function
#' @param pi.valid  individual expected profit lifts used as DV and weights
#'                  in the profit loss function
#' @param loss specify loss function and type of offer
#' @param reorder whether scores should be reordered
#' @param conditional whether the offer is conditional on renewal
#' @param miniter miniter hyperparameter
#' @param maxiter maxiter hyperparameter
#' @param rho learning rate
#' @param stoch use stochastic GB or regular GB
#' @param ratio fraction of observations to be sampled in case of stochastic GB
#' @param m.convergence.train   training scores to determine convergence by
#'                              calculating optimal campaign profit at each
#'                              iteration
#' @param m.convergence.valid   validation scores to determine convergence
#'                              by calculating optimal campaign profit at each
#'                              iteration
#' @param m.convergence.test    test scores to determine convergence by
#'                              calculating optimal campaign profit at each
#'                              iteration
#' @param delta.convergence     intervention cost to determine convergence by
#'                              calculating optimal campaign profit at each
#'                              iteration
#' @param increment.convergence granularity level to determine convergence by
#'                              calculating optimal campaign profit at each
#'                              iteration
#' @param m1.train targeted margin scores reordered classic loss train sample
#' @param m0.valid nontargeted margin scores reordered classic loss valid sample
#' @param m1.valid targeted margin scores reordered classic loss valid sample
#' @param m0.test  nontargeted margin scores reordered classic loss test sample
#' @param m1.test  targeted margin scores reordered classic loss test sample
#' @param verbose verbose output (default: TRUE)
#'
#' @return A list containing best iteration of sgb (best.iteration.valid),
#'         scores at best iteration (fmts),
#'         variables' importance (Variable.Importance),
#'         campaign profit at best target size for each iteration of SGB
#'         (maxlift.convergence.valid),
#'         target size at best iteration optimized on valid sample
#'         (targetsize.convergence.valid),
#'         and other SGB outcome measures.
#'
#' \itemize{
#'   \item iterations
#'   \item best.iteration.valid
#'   \item LogLik
#'   \item ytrain
#'   \item yv
#'   \item yts
#'   \item fmt
#'   \item fmv
#'   \item fmts
#'   \item Variable.Importance
#'   \item maxlift.convergence.valid
#'   \item targetsize.convergence.valid
#' }
#'
#' @export
sgb.loss <- function(
                     data.train,
                     data.valid,
                     data.test,
                     otherdata1,
                     otherdata2,
                     otherdata3,
                     otherdata4,
                     otherdata5,
                     otherdata6,

                     y.type,
                     pos.y,
                     pos.covariates,

                     pi,
                     pi.valid,

                     loss,
                     reorder,
                     conditional,

                     miniter,
                     maxiter,
                     rho,
                     stoch,
                     ratio,

                     m.convergence.train,
                     m.convergence.valid,
                     m.convergence.test,
                     delta.convergence,
                     increment.convergence,

                     m0.train,
                     m1.train,
                     m0.valid,
                     m1.valid,
                     m0.test,
                     m1.test,

                     verbose = TRUE) {

  # Define loss functions, including DV, weighting schemes and margin, and
  # derive likelihoods, gradients and hessians
  weightfunc <- function(myloss, myyt, mypi) {
    if (myloss == "classic") {
      return(weights = rep(1, length(myyt)))
    }
    if (myloss == "equal weighting") {
      return(weights = rep(1, length(myyt)))
    }
    if (myloss == "symmetric weighting") {
      return(weights = abs(mypi))
    }
    if (myloss == "right weighting") {
      return(weights = abs(mypi) * (mypi > 1) + (mypi <= 1))
    }
    if (myloss == "left weighting") {
      return(weights = abs(mypi) * (mypi < (-1)) + (mypi >= -1))
    }
  }

  marginfunc <- function(myloss, myyt, mypi, myfm) {
    if (myloss == "classic") {
      return(margin = myfm * myyt)
    }
    if (myloss != "classic") {
      return(margin = myfm * mypi)
    }
  }

  dvfunc <- function(myloss, myyt, mypi) {
    if (myloss == "classic") {
      return(dv = myyt)
    }
    if (myloss != "classic") {
      return(dv = mypi)
    }
  }

  likfunc <- function(myloss, myyt, mypi, myfm) {
    weights <- weightfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi
    )
    margin <- marginfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi,
      myfm = myfm
    )
    return(-log(1 + exp(-2 * margin)) * weights)
  }

  gradfunc <- function(myloss, myyt, mypi, myfm) {
    weights <- weightfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi
    )
    margin <- marginfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi,
      myfm = myfm
    )
    dv <- dvfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi
    )
    return(2 * dv * weights / (1 + exp(2 * margin)))
  }

  hessfunc <- function(myloss, myyt, mypi, myfm) {
    weights <- weightfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi
    )
    margin <- marginfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi,
      myfm = myfm
    )
    dv <- dvfunc(
      myloss = myloss,
      myyt = myyt,
      mypi = mypi
    )
    return((2 * dv)^2 * weights /
             ((1 + exp(-2 * margin)) * (1 + exp(2 * margin))))
  }

  y <- data.train[, pos.y]
  yvalid <- data.valid[, pos.y]
  ytest <- data.test[, pos.y]
  ntrain <- dim(data.train)[1]
  nvalid <- dim(data.valid)[1]
  ntest <- dim(data.test)[1]

  if (sum(colnames(data.train) == "w") != 0) {
    w <- data.train$w
  }

  # the following data are used for reordering scores (re-ordered classic loss)
  if (exists("otherdata1") & !is.null(otherdata1)) {
    yother1 <- otherdata1[, pos.y]
    nother1 <- dim(otherdata1)[1]
  }
  if (exists("otherdata2") & !is.null(otherdata2)) {
    yother2 <- otherdata2[, pos.y]
    nother2 <- dim(otherdata2)[1]
  }
  if (exists("otherdata3") & !is.null(otherdata3)) {
    yother3 <- otherdata3[, pos.y]
    nother3 <- dim(otherdata3)[1]
  }
  if (exists("otherdata4") & !is.null(otherdata4)) {
    yother4 <- otherdata4[, pos.y]
    nother4 <- dim(otherdata4)[1]
  }
  if (exists("otherdata5") & !is.null(otherdata5)) {
    yother5 <- otherdata5[, pos.y]
    nother5 <- dim(otherdata5)[1]
  }
  if (exists("otherdata6") & !is.null(otherdata6)) {
    yother6 <- otherdata6[, pos.y]
    nother6 <- dim(otherdata6)[1]
  }

  if (y.type == "zero/one") {
    yt <- 2 * y - 1
    yt.valid <- 2 * yvalid - 1
  } # transform the {0,1} dependent variable into a {-1,+1}
  if (y.type == "-one/one") {
    yt <- y
    yt.valid <- yvalid
  }

  p1 <- mean((yt + 1) / 2)

  # ====================================
  # STEP 1: Initializing the scores F_0
  # ====================================

  # starting values for p1 being the population mean of the {0,1}
  # dependent variable y
  fm <- array(log(p1 / (1 - p1)) / 2, c(ntrain, 1))
  # the scores take any value between -Inf and +Inf
  fmvalid <- array(log(p1 / (1 - p1)) / 2, c(nvalid, 1))
  fmtest <- array(log(p1 / (1 - p1)) / 2, c(ntest, 1))

  if (exists("otherdata1") & !is.null(otherdata1)) {
    fmother1 <- array(log(p1 / (1 - p1)) / 2, c(nother1, 1))
  }
  if (exists("otherdata2") & !is.null(otherdata2)) {
    fmother2 <- array(log(p1 / (1 - p1)) / 2, c(nother2, 1))
  }
  if (exists("otherdata3") & !is.null(otherdata3)) {
    fmother3 <- array(log(p1 / (1 - p1)) / 2, c(nother3, 1))
  }
  if (exists("otherdata4") & !is.null(otherdata4)) {
    fmother4 <- array(log(p1 / (1 - p1)) / 2, c(nother4, 1))
  }
  if (exists("otherdata5") & !is.null(otherdata5)) {
    fmother5 <- array(log(p1 / (1 - p1)) / 2, c(nother5, 1))
  }
  if (exists("otherdata6") & !is.null(otherdata6)) {
    fmother6 <- array(log(p1 / (1 - p1)) / 2, c(nother6, 1))
  }

  LLval <- c()
  # LLval.valid = c()
  maxlift.convergence.valid <- targetsize.convergence.valid <- c()


  fm.matrix <- fmvalid.matrix <- fmtest.matrix <- c()
  fmother1.matrix <- fmother2.matrix <- fmother3.matrix <-
    fmother4.matrix <- fmother5.matrix <- fmother6.matrix <- c()

  # set Variables Importance components
  Importance <- c()
  nvar <- ncol(data.train[, pos.covariates])
  Importancename <- colnames(data.train[, pos.covariates])[1:nvar]

  # set formula
  my.form <- as.formula("grad ~ .")

  iter <- 1
  plot.count <- 0
  converged <- 100
  set.seed(10)

  # ==============================================================
  # STEP 2.0: Start iterative procedure to find optimal scores F_m
  # ==============================================================

  iterationtoprint <- seq(1, 10000, by = 50)

  while (converged > 0.001) {
    if (iter > maxiter) {
      break
    }
    if (sum(iterationtoprint == iter) != 0) {
      print(iter)
    }
    llik <- likfunc(
      myloss = loss,
      myyt = yt,
      mypi = pi,
      myfm = fm
    )
    if (sum(iterationtoprint == iter) != 0) {
      print(sum(llik))
    }

    # ===============================================
    # STEP 2.1.: Subsample selection: Stochastic part
    # ===============================================

    if (stoch == TRUE) {
      set.seed(iter)
      sub <- sample(seq(1, ntrain),
        size = ratio * ntrain,
        replace = FALSE
      )
    } # position of the observations taken in the subsample
    if (stoch == FALSE) {
      set.seed(iter)
      sub <- sample(seq(1, ntrain), size = ntrain, replace = FALSE)
    }

    # ========================================
    # STEP 2.2.: Decision tree on the gradient
    # ========================================

    # calculate the gradient
    grad <- gradfunc(
      myloss = loss,
      myyt = yt[sub],
      mypi = pi[sub],
      myfm = fm[sub]
    )

    knodetree <- tree(my.form,
      as.data.frame(data.train[sub, pos.covariates]),
      control = tree.control(dim(data.train[sub, ])[1],
                             minsize = 10, mindev = 0.005)
    )

    # Limit the number of terminal nodes to maximum 8
    if (length(levels(factor(knodetree$where))) > 8) {
      knodetree <- prune.tree(knodetree, best = 8)
    }

    # variables' importance
    u <- knodetree$frame # is a data frame with a row for each node,
    # and row.names giving the node numbers.
    # The columns include var, the variable used at the split
    # (or "<leaf>" for a terminal node),
    # n, the (weighted) number of cases reaching that node,
    # dev the deviance of the node,
    # yval, the fitted value at the node
    # split, a two-column matrix of the labels for the left and right
    # splits at the node.

    nodelabels <- dimnames(u)[[1]]   # VECTOR OF NODE NUMBERS,
                                     # including terminal nodes
    udev <- as.data.frame(u$dev)     # DEVIANCE OF THE NODE
    row.names(udev) <- nodelabels
    labeln <- as.numeric(nodelabels) # transform nodes names into numbers,
                                     # e.g. "1" is 1
    label2 <- 2 * labeln
    label2 <- as.character(label2)
    label3 <- 2 * labeln + 1
    label3 <- as.character(label3)

    # deviance parent node - sum deviance 2 children nodes
    gaindev <- udev[nodelabels, ] - udev[label2, ] - udev[label3, ]

    # gaindev has same length as number of nodes
    xhelp <- data.frame(u$var, gaindev)

    Importance.temp <- array(NA, c(nvar, 1))
    for (i in (1:nvar))
    {
      loglist <- (xhelp[, 1] == Importancename[i])
      # importance per variable
      Importance.temp[i] <- sum(xhelp[loglist, 2])
    }
    Importance <- cbind(
      Importance,
      Importance.temp / sum(Importance.temp, na.rm = TRUE)
    )

    # Predict the node where an observation falls into:

    # in the full training set
    treetrainselected <- predict(knodetree, data.train[sub, ], type = "where")
    # in the full training set
    treetrain <- predict(knodetree, data.train, type = "where")
    # in the validation set
    treevalid <- predict(knodetree, data.valid, type = "where")
    # in the test set
    treetest <- predict(knodetree, data.test, type = "where")

    if (exists("otherdata1") & !is.null(otherdata1)) {
      treeother1 <- predict(knodetree, otherdata1, type = "where")
    }
    if (exists("otherdata2") & !is.null(otherdata2)) {
      treeother2 <- predict(knodetree, otherdata2, type = "where")
    }
    if (exists("otherdata3") & !is.null(otherdata3)) {
      treeother3 <- predict(knodetree, otherdata3, type = "where")
    }
    if (exists("otherdata4") & !is.null(otherdata4)) {
      treeother4 <- predict(knodetree, otherdata4, type = "where")
    }
    if (exists("otherdata5") & !is.null(otherdata5)) {
      treeother5 <- predict(knodetree, otherdata5, type = "where")
    }
    if (exists("otherdata6") & !is.null(otherdata6)) {
      treeother6 <- predict(knodetree, otherdata6, type = "where")
    }

    # indice is (kk x ntrain) matrix with 1/0 indicator variables for
    # belonging to one of the terminal nodes.
    # kk=number of terminal nodes
    kk <- length(levels(factor(treetrainselected)))

    if (stoch == TRUE) {
      indice <- array(0, c(kk, (ntrain * ratio)))
    } # subsample of the training set
    if (stoch == FALSE) {
      indice <- array(0, c(kk, ntrain))
    }

    indiceall <- array(0, c(kk, (ntrain))) # training set
    indicev <- array(0, c(kk, nvalid))     # validation set
    indicet <- array(0, c(kk, ntest))      # test set

    indiceo1 <- array(0, c(kk, nother1))
    indiceo2 <- array(0, c(kk, nother2))
    indiceo3 <- array(0, c(kk, nother3))
    indiceo4 <- array(0, c(kk, nother4))
    indiceo5 <- array(0, c(kk, nother5))
    indiceo6 <- array(0, c(kk, nother6))

    for (k in 1:kk) {
      indice[k, ] <- (treetrainselected == (levels(factor(
        treetrainselected
      ))[k]))
      indiceall[k, ] <- (treetrain == (levels(factor(
        treetrainselected
      ))[k]))
      indicev[k, ] <- (treevalid == (levels(factor(
        treetrainselected
      ))[k]))
      indicet[k, ] <- (treetest == (levels(factor(
        treetrainselected
      ))[k]))

      if (exists("otherdata1") & !is.null(otherdata1)) {
        indiceo1[k, ] <- (treeother1 == (levels(factor(
          treetrainselected
        ))[k]))
      }
      if (exists("otherdata2") & !is.null(otherdata2)) {
        indiceo2[k, ] <- (treeother2 == (levels(factor(
          treetrainselected
        ))[k]))
      }
      if (exists("otherdata3") & !is.null(otherdata3)) {
        indiceo3[k, ] <- (treeother3 == (levels(factor(
          treetrainselected
        ))[k]))
      }
      if (exists("otherdata4") & !is.null(otherdata4)) {
        indiceo4[k, ] <- (treeother4 == (levels(factor(
          treetrainselected
        ))[k]))
      }
      if (exists("otherdata5") & !is.null(otherdata5)) {
        indiceo5[k, ] <- (treeother5 == (levels(factor(
          treetrainselected
        ))[k]))
      }
      if (exists("otherdata6") & !is.null(otherdata6)) {
        indiceo6[k, ] <- (treeother6 == (levels(factor(
          treetrainselected
        ))[k]))
      }
    }

    # ==========================================================================
    # STEP 2.3.: Optimization of gamma per terminal node using Netwon Raphson
    # ==========================================================================

    # vector of length kk
    gamma <- array(0, c(kk, 1))

    for (k in 1:kk)
    # each node separately
    {
      diff <- 1
      # position of observations taken in the subsample and falling at node k
      ytk <- yt[sub * indice[k, ]]
      if (loss != "classic") {
        pik <- pi[sub * indice[k, ]]
      }
      fmk <- fm[sub * indice[k, ]]

      # Gradient calculation
      gradk <- gradfunc(
        myloss = loss,
        myyt = ytk,
        mypi = pik,
        myfm = fmk
      )

      # Hessian calculation
      hessk <- hessfunc(
        myloss = loss,
        myyt = ytk,
        mypi = pik,
        myfm = fmk
      )

      # Log Likelihood
      loglikold <- sum(likfunc(
        myloss = loss,
        myyt = ytk,
        mypi = pik,
        myfm = fmk
      ))

      fmkold <- fmk
      stopvalue <- 0.00001

      while (diff > stopvalue) {
        step <- sum(gradk) / sum(hessk)
        fmk <- fmk + step

        if (loss == "classic") {
          if (abs(sum((log(
            1 + exp(-2 * ytk * fmk)
          )))) != Inf & abs(loglikold) != Inf &
            is.nan(abs(sum((
              log(1 + exp(-2 * ytk * fmk))
            )))) == FALSE) {
            logliknew <- sum(likfunc(
              myloss = loss,
              myyt = ytk,
              mypi = pik,
              myfm = fmk
            ))
            diff <- logliknew - loglikold
          }
          if (abs(sum((log(
            1 + exp(-2 * ytk * fmk)
          )))) == Inf | abs(loglikold) == Inf |
            is.nan(abs(sum((
              log(1 + exp(-2 * ytk * fmk))
            ))))) {
            diff <- stopvalue
          }
        }

        if (loss != "classic") {
          if (abs(sum((log(
            1 + exp(-2 * pik * fmk)
          )))) != Inf & abs(loglikold) != Inf &
            is.nan(abs(sum((
              log(1 + exp(-2 * pik * fmk))
            )))) == FALSE) {
            logliknew <- sum(likfunc(
              myloss = loss,
              myyt = ytk,
              mypi = pik,
              myfm = fmk
            ))
            diff <- logliknew - loglikold
          }
          if (abs(sum((log(
            1 + exp(-2 * pik * fmk)
          )))) == Inf | abs(loglikold) == Inf |
            is.nan(abs(sum((
              log(1 + exp(-2 * pik * fmk))
            ))))) {
            diff <- stopvalue
          }
        }

        # Gradient calculation
        gradk <- gradfunc(
          myloss = loss,
          myyt = ytk,
          mypi = pik,
          myfm = fmk
        )

        # Hessian calculation
        hessk <- hessfunc(
          myloss = loss,
          myyt = ytk,
          mypi = pik,
          myfm = fmk
        )
        if (sum(hessk) == 0 |
          is.na(sum(hessk))) {
          diff <- stopvalue
        }

        loglikold <- logliknew
      }
      # gamma = difference between latest fm (updated)& first fm
      a <- fmk - fmkold
      # gamma[k]=gammak
      gamma[k] <- a[1]

    } ### End Newton-Raphson

    indiceall <- t(indiceall)
    fm[sub] <- fm[sub] + rho * (indiceall[sub, ] %*% gamma) # update subsample

    if (stoch == TRUE) {
      set.seed(iter)
      sub <- sample(seq(1, nvalid),
        size = ratio * nvalid,
        replace = FALSE
      )
    }
    if (stoch == FALSE) {
      set.seed(iter)
      sub <- sample(seq(1, nvalid), size = nvalid, replace = FALSE)
    }
    fmvalid[sub] <- fmvalid[sub] + rho * (t(indicev)[sub, ] %*% gamma)

    if (stoch == TRUE) {
      set.seed(iter)
      sub <- sample(seq(1, ntest),
        size = ratio * ntest,
        replace = FALSE
      )
    }
    if (stoch == FALSE) {
      set.seed(iter)
      sub <- sample(seq(1, ntest), size = ntest, replace = FALSE)
    }
    fmtest[sub] <- fmtest[sub] + rho * (t(indicet)[sub, ] %*% gamma)

    if (exists("otherdata1") & !is.null(otherdata1)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother1),
          size = ratio * nother1,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother1), size = nother1, replace = FALSE)
      }
      fmother1[sub] <- fmother1[sub] + rho * (t(indiceo1)[sub, ] %*% gamma)
    }

    if (exists("otherdata2") & !is.null(otherdata1)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother2),
          size = ratio * nother2,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother2), size = nother2, replace = FALSE)
      }
      fmother2[sub] <- fmother2[sub] + rho * (t(indiceo2)[sub, ] %*% gamma)
    }

    if (exists("otherdata3") & !is.null(otherdata1)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother3),
          size = ratio * nother3,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother3), size = nother3, replace = FALSE)
      }
      fmother3[sub] <- fmother3[sub] + rho * (t(indiceo3)[sub, ] %*% gamma)
    }

    if (exists("otherdata4") & !is.null(otherdata4)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother4),
          size = ratio * nother4,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother4), size = nother4, replace = FALSE)
      }
      fmother4[sub] <- fmother4[sub] + rho * (t(indiceo4)[sub, ] %*% gamma)
    }

    if (exists("otherdata5") & !is.null(otherdata5)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother5),
          size = ratio * nother5,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother5), size = nother5, replace = FALSE)
      }
      fmother5[sub] <- fmother5[sub] + rho * (t(indiceo5)[sub, ] %*% gamma)
    }

    if (exists("otherdata6") & !is.null(otherdata6)) {
      if (stoch == TRUE) {
        set.seed(iter)
        sub <- sample(seq(1, nother6),
          size = ratio * nother6,
          replace = FALSE
        )
      }
      if (stoch == FALSE) {
        set.seed(iter)
        sub <- sample(seq(1, nother6), size = nother6, replace = FALSE)
      }
      fmother6[sub] <- fmother6[sub] + rho * (t(indiceo6)[sub, ] %*% gamma)
    }

    # Compute various fit and prediction criteria
    LLval <- c(LLval, sum(likfunc(
      myloss = loss,
      myyt = yt,
      mypi = pi,
      myfm = fm
    )))
    if (abs(LLval[iter]) == Inf |
      is.na(abs(LLval[iter]))) {
      iter <- iter + 1
      break
    }

    if (verbose == TRUE & sum(iterationtoprint == iter) != 0) {
      par(mfrow = c(1, 1))
      graphics::plot(LLval,
        type = "l",
        xlab = "Iterations",
        ylab = "Log-Likelihood"
      )
    }

    iter <- iter + 1

    #---------------------------------------------------------------------------
    # Evaluate if model converges by computing campaign profit at each iteration
    #---------------------------------------------------------------------------

    # to avoid that reordered scores will be used at next iteration of the SGB
    fmvalid.saved <- fmvalid
    if (reorder == TRUE) {
      r0.valid.post <- 1 / (1 + exp(fmother3))
      r1.valid.post <- 1 / (1 + exp(fmother4))
      if (conditional == TRUE) {
        fmvalid <- ((1 - r1.valid.post) * (m1.valid - delta.convergence)) -
          ((1 - r0.valid.post) * m0.valid)
      }
      if (conditional == FALSE) {
        fmvalid <- ((1 - r1.valid.post) * (m1.valid)) -
          ((1 - r0.valid.post) * m0.valid) - delta.convergence
      }
    }

    # Determine optimal target size on the validation sample in order to check
    # convergence in campaign profit across iterations
    TSO <- targetsizeoptimization(
      y = data.valid[, pos.y],
      treated = data.valid$w,
      scores = fmvalid,
      m = m.convergence.valid,
      delta = delta.convergence,
      conditional = conditional,
      increment = increment.convergence,
      plot = FALSE
    )
    maxlift.convergence.valid    <- c(maxlift.convergence.valid,
                                      TSO$maxprofit.trimmed)

    targetsize.convergence.valid <- c(targetsize.convergence.valid,
                                      TSO$targetsize.maxprofit.trimmed)

    # Convergence check on validation sample
    if (iter > miniter) {
      converged <- mean(maxlift.convergence.valid[(iter - 200):(iter - 1)]) -
        mean(maxlift.convergence.valid[(iter - 400):(iter -
          201)])
    }

    fmvalid <- fmvalid.saved

    # save all predictions for final selection of best iteration
    # outside the loop
    fm.matrix <- cbind(fm.matrix, fm)
    fmvalid.matrix <- cbind(fmvalid.matrix, fmvalid)
    fmtest.matrix <- cbind(fmtest.matrix, fmtest)
    fmother1.matrix <- cbind(fmother1.matrix, fmother1)
    fmother2.matrix <- cbind(fmother2.matrix, fmother2)
    fmother3.matrix <- cbind(fmother3.matrix, fmother3)
    fmother4.matrix <- cbind(fmother4.matrix, fmother4)
    fmother5.matrix <- cbind(fmother5.matrix, fmother5)
    fmother6.matrix <- cbind(fmother6.matrix, fmother6)
  }
  # end iterative sgb procedure

  # Best iteration on valid sample
  best.iteration.valid <-
    (1:iter)[maxlift.convergence.valid == max(maxlift.convergence.valid,
                                              na.rm = TRUE
                                              )][1]

  # save predictions at best iteration
  fm <- fm.matrix[, best.iteration.valid]
  fmvalid <- fmvalid.matrix[, best.iteration.valid]
  fmtest <- fmtest.matrix[, best.iteration.valid]
  fmother1 <- fmother1.matrix[, best.iteration.valid]
  fmother2 <- fmother2.matrix[, best.iteration.valid]
  fmother3 <- fmother3.matrix[, best.iteration.valid]
  fmother4 <- fmother4.matrix[, best.iteration.valid]
  fmother5 <- fmother5.matrix[, best.iteration.valid]
  fmother6 <- fmother6.matrix[, best.iteration.valid]

  # save predictions in case of re-ordering
  if (reorder == TRUE) {
    r0.train.post <- 1 / (1 + exp(fmother1))
    r1.train.post <- 1 / (1 + exp(fmother2))
    r0.valid.post <- 1 / (1 + exp(fmother3))
    r1.valid.post <- 1 / (1 + exp(fmother4))
    r0.test.post <- 1  / (1 + exp(fmother5))
    r1.test.post <- 1  / (1 + exp(fmother6))

    if (conditional == TRUE) {
      fm <- ((1 - r1.train.post) * (m1.train - delta)) -
        ((1 - r0.train.post) * m0.train)
      fmvalid <- ((1 - r1.valid.post) * (m1.valid - delta)) -
        ((1 - r0.valid.post) * m0.valid)
      fmtest <- ((1 - r1.test.post) * (m1.test - delta)) -
        ((1 - r0.test.post) * m0.test)
    }
    if (conditional == FALSE) {
      fm <- ((1 - r1.train.post) * (m1.train)) -
        ((1 - r0.train.post) * m0.train) - delta
      fmvalid <- ((1 - r1.valid.post) * (m1.valid)) -
        ((1 - r0.valid.post) * m0.valid) - delta
      fmtest <- ((1 - r1.test.post) * (m1.test)) -
        ((1 - r0.test.post) * m0.test) - delta
    }
  }

  # save target size at best iteration
  targetsize.convergence.valid <-
    targetsize.convergence.valid[best.iteration.valid]

  # variables' importance at best iteration
  rownames(Importance) <- Importancename
  if (best.iteration.valid > 1) {
    TotImportance <- apply(Importance[, 1:best.iteration.valid], 1, mean)
  }

  if (best.iteration.valid == 1) {
    TotImportance <- as.vector(Importance[, 1])
    names(TotImportance) <- rownames(Importance)
  }

  ordering         <- rev(order(TotImportance))
  TotImportance    <- TotImportance[ordering]
  TotImportanceALL <- array(TotImportance,
                            c(nvar, 1),
                            dimnames = list(Importancename[ordering], ""))

  list(
    iterations                   = iter - 1,
    best.iteration.valid         = best.iteration.valid,
    LogLik                       = LLval,
    ytrain                       = y,
    yv                           = yvalid,
    yts                          = ytest,
    fmt                          = fm,
    fmv                          = fmvalid,
    fmts                         = fmtest,
    Variable.Importance          = TotImportanceALL,
    maxlift.convergence.valid    = maxlift.convergence.valid,
    targetsize.convergence.valid = targetsize.convergence.valid
  )
}
