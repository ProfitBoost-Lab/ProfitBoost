Sys.setenv("R_TESTS" = "")

library(testthat)
library(ProfitBoost)

test_check("ProfitBoost")
