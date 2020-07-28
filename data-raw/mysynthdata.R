# data-raw/mysynthdata.R
# Data import and processing pipeline

# load raw data
mysynthdata <- read.csv("data-raw/mysynthdata.csv")

# drop index
mysynthdata[,1] = NULL

# set to factor
mysynthdata[,2] <- as.factor(mysynthdata[,2])
mysynthdata[,3] <- as.factor(mysynthdata[,3])
mysynthdata[,4] <- as.factor(mysynthdata[,4])
mysynthdata[,5] <- as.factor(mysynthdata[,5])
mysynthdata[,6] <- as.factor(mysynthdata[,6])
mysynthdata[,12] <- as.factor(mysynthdata[,12])

usethis::use_data(mysynthdata, overwrite = TRUE)

