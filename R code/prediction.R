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


## Contiguity Neighbors

csa.shp <-  rgdal::readOGR(file.path("wip", "health"), "health") 
coord.shp <- coordinates(csa.shp)

W_cont_el <- poly2nb(csa.shp, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)



## Non-Contiguity Neighbors (snap distance = 100,000km)

W_cont_s <- poly2nb(csa.shp, queen=F, snap= km2d(100000))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)





## Prediction

#Convert to spdataframe
coordinates(csa.data.anal) <- ~lon.med.avg + lat.med.avg
# coordinates(my_csa) <- ~lon + lat
coordinates(dat$train_data) <- ~lon + lat


# bw <- bw.gwr(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
#                abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
#                narc,data = my_csa, approach="CV",kernel="gaussian",
#              adaptive=F, p=2, theta=0, longlat=T)
# 
# 
# my_csa %>% data.frame() %>%
#   subset(select = c(csa,abshs) ) %>%
#   inner_join(my_block) -> my_block

# 
# coordinates(my_block) <- ~lon + lat
# pred.b <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
#                         abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
#                         narc,data = my_csa, 
#                       predictdata = my_block,
#                       bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

bw <- bw.gwr(scale(lifeexp,scale = F) ~ propfemhh + propbelow.pred + susp + liquor + elheat,
             data = dat$train_data, approach="CV",kernel="gaussian",
             adaptive=F, p=2, theta=0, longlat=T)
coordinates(my_block) <- ~lon + lat
pred.b <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + propbelow.pred + susp + liquor + elheat,
                      data = dat$train_data, 
                      predictdata = my_block,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)


# data.frame(my_block, pred = pred.b$SDF$prediction + 73.76) %>%
#   group_by(csa) %>%
#   summarise(lifeexp = mean(pred)) -> csa_pred

m <- mean(dat$train_data$lifeexp)
data.frame(my_block, pred = pred.b$SDF$prediction + m) %>%
  group_by(csa) %>%
  summarise(lifeexp.pred = mean(pred)) -> csa_pred

csa_pred %>%
  inner_join(dat$test_data[,1:2]) -> test_train.csa

test_train.csa

# data.frame(my_csa@data[,1:2], type = "Observed", index = c(1:55)) %>%
#   bind_rows(data.frame(csa_pred, type = "Predicted aggregate", index = c(1:55))) %>% 
#   ggplot(aes(x = index, y  = lifeexp, group = type, colour = type)) + geom_line() + 
#   ylab("Life expectancy") + xlab("CSA") +
#   ggtitle("Plot showing the fit of each \nmodel compared to the observed life expectancy values")


## Delta method

# n <- NROW(my_block_raw)
# my_csa %>% data.frame() %>%
#   subset(select = c(csa,abshs, lifeexp) ) %>%
#   inner_join(my_block_raw) %>%
#   arrange(csa,neighborhood,block)-> my_block_raw
# my_block_raw$pred <- NA
# 
# 
# 
# for (i in 1:n){
#   #Remove all values relating to block i
#   dat <- my_block_raw[-i,-22]
# 
#   #Aggregate to csa without block i
#   dat %>%
#     dplyr::select(-neighborhood, -block) %>%
#     group_by(csa) %>%
#     summarise_all(mean) -> my_dat
# 
#   #Center and scale the aggregated variables
#   my_dat <- data.frame(my_dat[,c(1,3)],apply(my_dat[,-c(1,3,7:8)], 2, scale), my_dat[,7:8])
# 
#   #Convert to spatialpointdataframe
#   coordinates(my_dat) <- ~lon + lat
# 
#   #Get the prediction at the CSA realting to block i
#   pred <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant +
#                         abshs + susp + liquor + heatgas + elheat + wlksc + racdiv +
#                         narc,data = my_csa,
#                       predictdata = my_dat,
#                       bw = bw, kernel = "gaussian",adaptive = F, longlat = T)
# 
#   #Create a new dataset where the i'th prediction at the CSA level. I am looping over all the blocks in a particular csa
#   my_block_raw[,-22] %>%
#     inner_join(data.frame(csa = my_dat$csa,pred = pred$SDF$prediction)) -> output
#   my_block_raw$pred[i] <- output$pred[i]
# }
# 
# pred.csa <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant +
#                       abshs + susp + liquor + heatgas + elheat + wlksc + racdiv +
#                       narc,data = my_csa,
#                     bw = bw, kernel = "gaussian",adaptive = F, longlat = T)
# 
# data.frame(csa = my_csa@data[,1], pred.csa = pred.csa$SDF$prediction) %>%
#   inner_join(subset(my_block_raw, select = c(csa,neighborhood,block, pred, lifeexp))) %>% 
#   dplyr::select(csa,neighborhood,block, lifeexp, pred, pred.csa) %>% 
#   mutate(delta = pred.csa - pred, block.le = lifeexp + delta) %>% 
#   unique() -> block_pred
# 
# block_pred %>%
#   group_by(csa) %>%
#   summarise(lifeexp = mean(block.le)) -> block_pred_csa
# 
# print(data.frame(my_csa@data[,1:2], method = "Observed", index = c(1:55)) %>%
#   bind_rows(data.frame(csa_pred, method = "Transfer func", index = c(1:55))) %>%
#   bind_rows(data.frame(block_pred_csa, method = "Delta method", index = c(1:55))) %>%
#   ggplot(aes(x = index, y  = lifeexp, group = method, colour = method)) + geom_line() + 
#   ylab("Life expectancy") + xlab("CSA") +
#   ggtitle("Plot showing the fit of each \nmodel compared to the observed life expectancy values"))

#Using CV method I.e data

#Get blocks and neighborhoods in the csa test dataset
# my_block_cv <- my_block_raw[my_block_raw$csa %in% dat$test_data$csa, ]

#Get blocks and neighborhoods in the whole dataset
my_block_cv <- my_block_raw
n <- NROW(my_block_cv)

#Get lifeexpectancy for those csas and rearrange the columns
# dat$train_data %>% data.frame() %>%
#   subset(select = c(csa,lifeexp) ) %>%
#   inner_join(my_block_cv) %>%
#   arrange(csa,neighborhood,block)-> my_block_cv

arrange(my_block_cv,csa,neighborhood,block)-> my_block_cv
my_block_cv$pred <- NA



for (i in 1:n){
  #Remove all values relating to block i along with the NA column for pred
  dat_cv <- my_block_cv[-i,-20]
  
  #Aggregate to csa without block i
  dat_cv %>%
    dplyr::select(-neighborhood, -block) %>%
    group_by(csa) %>%
    summarise_all(mean) -> my_dat
  
  #Center and scale the aggregated variables
  my_dat <- data.frame(my_dat[,c(1)],apply(my_dat[,-c(1,5:6)], 2, scale), my_dat[,5:6])
  
  #Convert to spatialpointdataframe
  coordinates(my_dat) <- ~lon + lat
  
  #Get the prediction at the CSA realting to block i
  pred <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + propbelow.pred + susp + liquor + elheat, 
                      data = dat$train_data,
                      predictdata = my_dat,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)
  
  #Create a new dataset where the i'th prediction at the CSA level. I am looping over all the blocks in a particular csa
  my_block_cv[,-20] %>%
    inner_join(data.frame(csa = my_dat$csa,pred = pred$SDF$prediction)) -> output
  my_block_cv$pred[i] <- output$pred[i]
}

#Get blocks and neighborhoods in the csa test dataset
my_block_cv <- my_block_cv[my_block_cv$csa %in% dat$test_data$csa, ]
coordinates(dat$test_data) <- ~lon + lat

pred.csa <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + propbelow.pred + susp + liquor + elheat, 
                        data = dat$train_data, predictdata = dat$test_data,
                        bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

data.frame(csa = dat$test_data@data[,1], pred.csa = pred.csa$SDF$prediction, lifeexp = dat$test_data@data[,2]) %>%
  inner_join(subset(my_block_cv, select = c(csa,neighborhood,block, pred))) %>%
  dplyr::select(csa,neighborhood,block, lifeexp, pred, pred.csa) %>%
  mutate(delta = pred.csa - pred, block.le = lifeexp + delta) %>%
  unique() -> block.pred

#Aggregate the results
block.pred %>%
  dplyr::select(-neighborhood, -block) %>%
  group_by(csa) %>%
  summarise_all(mean) -> csa_block.pred

m = mean(dat$train_data@data$lifeexp)
csa_block.pred[,c(1,2,6,4)] %>%
  mutate(pred.csa = pred.csa+m ) -> csa_block.pred



# print(NROW(block_pred))
# ggsave(file.path("..","Plots","pred.png"), width = 45, height = 45, units = "cm")

detach()
unlink("analyses_data.RData")
setwd("..")