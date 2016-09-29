#Create all data files to be used during analysis
setwd(file.path("Data"))

#First dataset is Census. 1. 2010-2014 has missing info for 2010, 2012, and 2013
census10.14 <- readr::read_csv(file.path("raw_data", "census.csv"))
census10 <- readr::read_csv(file.path("raw_data", "census10.csv"))
census10.12 <- readr::read_csv(file.path("raw_data", "census12.csv"))
census10.13 <- readr::read_csv(file.path("raw_data", "census13.csv"))


setwd(file.path(".."))