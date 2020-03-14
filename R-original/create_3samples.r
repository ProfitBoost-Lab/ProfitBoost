#=========================================== #
# Randomly divide a dataset in three samples #
#=========================================== #

# mydata: input data to be divided in 3 samples
# size.train: number of observations in the calibration sample
# size.valid: number of observations in the validation sample
# size.test: number of observations in the test sample
# myseed: set seed for reproducibility

create.3samples = function (mydata,
                            size.train,
                            size.valid,
                            size.test,
                            myseed = 268237)
{
  RNGkind(sample.kind = "Rounding")  # for reproducible results w.r.t. older versions of R
  set.seed(myseed)
  randomization.index = sample(seq(1, nrow(mydata)), size = nrow(mydata), replace =
                                 FALSE)

  data.train = mydata[randomization.index[1:size.train], ]
  data.valid = mydata[randomization.index[(size.train + 1):(size.train +
                                                              size.valid)], ]

  if (size.train + size.valid < nrow(mydata))
  {
    data.test =  mydata[randomization.index[(size.train + size.valid + 1):nrow(mydata)], ]
  }
  if (size.train + size.valid >= nrow(mydata))
  {
    data.test =  NA
  }

  train.index = randomization.index[1:size.train]
  test.index  = randomization.index[(size.train + size.valid + 1):nrow(mydata)]

  return(
    list(
      data.train = data.train,
      data.valid = data.valid,
      data.test = data.test,
      train.index = train.index,
      test.index = test.index
    )
  )
}
