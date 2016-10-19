setwd(file.path("..", "Data"))


## Load spatial packages

library(sp)           ## Data management
library(dplyr)        ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(spgwr)        ## GWR
library(ggplot2)      ## Plotting
library(broom)        ## Data management
library(GWmodel)      ## Predict GWR
library(cvTools)      ## Compare fits using cross validation



attach("analyses_data.RData")

#Data formatting
names(csa.data.anal) <- gsub("[[:digit:]]", "", names(csa.data.anal))
names(block_data.anal) <- gsub("[[:digit:]]", "", names(block_data.anal))

BG_neighbhd_csa -> BG_neighbhd

names(BG_neighbhd_csa) <- tolower(names(BG_neighbhd_csa))
BG_neighbhd_csa[,c(2,3,6,9:13,15)] -> my_data

my_data %>%
  inner_join(block_data.anal[,-c(4,5,15:16)]) -> my_block

#Center and scall all the predictors
my_block_raw <- my_block
my_block <- data.frame(my_block[,c(10,1:2)],apply(my_block[,-c(1:2, 6:7,10)], 2, scale), my_block[,6:7])


BG_neighbhd_csa[,c(2,3,6,9:13,15)] -> my_data

my_data %>%
  inner_join(block_data.anal[,c(1:3)]) %>%
  dplyr::select(-neighborhood, -block) %>% 
  group_by(csa) %>%
  summarise_all(mean) -> my_data


my_data %>% 
  inner_join(csa.data.anal[,-c(4:6, 27:28)])-> my_csa

#Center and scale all the predictors

my_csa <- data.frame(my_csa[,c(1,9)],apply(my_csa[,-c(1, 9,5:6)], 2, scale), my_csa[,5:6])

rm(my_data)


#Randomly Divide Data into Training and Test Sets
get.test_train <- function(p = 0.3, data, seed = 1234){
  set.seed(seed = seed)
  n <- NROW(data)
  test <- sample(1:n, p*n)
  train <- !c(1:n) %in% test
  list(train_data = data[train,], test_data = data[test,])
}
get.test_train(0.30, data = my_csa, seed = 1234) -> dat

########
## GWR (WARNING: This may take a while to run)
########

#### Distance Conversion         ####
#####################################


## Function: Convert km to degrees
km2d <- function(km){
  out <- (km/1.852)/60
  return(out)
}


col.lm <- lm(lifeexp~.,data=dat$train_data[,-c(1,29:30)])
# col.lm <- lm(lifeexp~.,data=my_csa[,-c(1,29:30)])


# Stepwise using AIC
st1 <- step(col.lm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.lm),
            trace = 0)
st3 <- step(col.lm,trace = 0)
# Stepwise using BIC
n <- NROW(dat$train_data)
# n <- NROW(my_csa)
st2 <- step(col.lm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.lm),
            trace = 0, k = log(n))

st4 <- step(col.lm, trace = 0, k = log(n))

# set up folds for cross-validation
set.seed(1234)
folds <- cvFolds(nrow(dat$train_data), K = 5, R = 10)
# folds <- cvFolds(nrow(my_csa), K = 5, R = 10)

cvFitLm1 <- cvLm(st1, cost = rtmspe, 
                 folds = folds, trim = 0.1)
cvFitLm2 <- cvLm(st2, cost = rtmspe, 
                 folds = folds, trim = 0.1)
cvFitLm3 <- cvLm(st3, cost = rtmspe, 
                 folds = folds, trim = 0.1)
cvFitLm4 <- cvLm(st4, cost = rtmspe, 
                 folds = folds, trim = 0.1)

print(cvSelect(st1 = cvFitLm1, st2 = cvFitLm2, st3 = cvFitLm3, st4 = cvFitLm4))

save.image("analyses_data.RData")
detach()
setwd("..")