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


# col.lm <- lm(lifeexp~.,data=dat$train_data[,-c(1,29:30)])
col.lm <- lm(lifeexp~.,data=my_csa[,-c(1,29:30)])


# Stepwise using AIC
st1 <- step(col.lm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.lm),
            trace = 0)
st3 <- step(col.lm,trace = 0)
# Stepwise using BIC
# n <- NROW(dat$train_data)
n <- NROW(my_csa)
st2 <- step(col.lm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.lm),
            trace = 0, k = log(n))

st4 <- step(col.lm, trace = 0, k = log(n))

# set up folds for cross-validation
folds <- cvFolds(nrow(my_csa), K = 5, R = 10)

cvFitLm1 <- cvLm(st1, cost = rtmspe, 
                folds = folds, trim = 0.1)
cvFitLm2 <- cvLm(st2, cost = rtmspe, 
                folds = folds, trim = 0.1)
cvFitLm3 <- cvLm(st3, cost = rtmspe, 
                 folds = folds, trim = 0.1)
cvFitLm4 <- cvLm(st4, cost = rtmspe, 
                 folds = folds, trim = 0.1)

cvSelect(st1 = cvFitLm1, st2 = cvFitLm2, st3 = cvFitLm3, st4 = cvFitLm4)

col.lm <- lm(scale(lifeexp, scale = F) ~ propfemhh + totalincidents + prop.vacant + 
               abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
               narc, data = my_csa)
m <- mean(my_csa$lifeexp)


resids<-residuals(col.lm)
colours <- c("dark blue", "blue", "red", "dark red") 

map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(coord.lon_lat[,2],coord.lon_lat[,3])) 

## Contiguity Neighbors

csa.shp <-  rgdal::readOGR(file.path("wip", "health"), "health") 
coord.shp <- coordinates(csa.shp)

W_cont_el <- poly2nb(csa.shp, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)
# print(W_cont_el_mat, zero.policy=TRUE) 

#NO spatial autocorrelation!
moran.test(resids,W_cont_el_mat, zero.policy = T)
lm.morantest(col.lm, W_cont_el_mat, zero.policy = T)
moran.mc(resids,W_cont_el_mat, zero.policy = T, nsim  = 999)

## Non-Contiguity Neighbors (snap distance = 100,000km)

W_cont_s <- poly2nb(csa.shp, queen=F, snap= km2d(100000))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)
# print(W_cont_s_mat, zero.policy=TRUE) 

#Again NO spatial autocorrelation!
moran.test(resids,listw = W_cont_s_mat, zero.policy = T)


#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your CSA dataframe and plot using geom_point in ggplot2
# spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 




## GWR

#Convert to spdataframe
coordinates(csa.data.anal) <- ~lon.med.avg + lat.med.avg
coordinates(my_csa) <- ~lon + lat


#Prediction


bw <- bw.gwr(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
               abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
               narc,data = my_csa, approach="CV",kernel="gaussian",
             adaptive=F, p=2, theta=0, longlat=T)


my_csa %>% data.frame() %>%
  subset(select = c(csa,abshs) ) %>%
  inner_join(my_block) -> my_block


coordinates(my_block) <- ~lon + lat
pred.b <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
                        abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
                        narc,data = my_csa, 
                      predictdata = my_block,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

data.frame(my_block, pred = pred.b$SDF$prediction + 73.76) -> my_block

my_block %>%
  group_by(csa) %>%
  summarise(pred = mean(pred)) -> csa_pred


plot(my_csa$lifeexp, type = "l", col = "red")
lines(csa_pred$pred, col = "blue")

## Comparing the models
#Plot
data.frame("Life_expectancy" = mod.gwr3$lm$y, Model = "Observed", CSA = 1:55) %>%
  bind_rows(data.frame("Life_expectancy" = lg.fit$fitted.values + m, Model = "SALM Pred", CSA = 1:55)) %>%
  bind_rows(data.frame("Life_expectancy" = mod.gwr3$lm$fitted.values, Model = "GWR Pred", CSA = 1:55)) %>%
  ggplot(aes(x= CSA, y = Life_expectancy, group = Model, colour = Model)) + geom_line() + 
  ylab("Life expectancy") + ggtitle("Plot showing the fit of each \nmodel compared to the observed life expectancy values")

ggsave(file.path("..", "Plots","fits.png"))



# gwr.morantest(mod.gwr, W_cont_el_mat, zero.policy = T)
# gwr.morantest(mod.gwr1, W_cont_el_mat, zero.policy = T)
# 
# moran.test(mod.gwr1$SDF$gwr.e, W_cont_el_mat, zero.policy = T)
# moran.test(mod.gwr$SDF$gwr.e, W_cont_el_mat, zero.policy = T)
moran.test(mod.gwr3$SDF$working_resids, W_cont_el_mat, zero.policy = T)
moran.test(lg.fit$residuals, W_cont_el_mat, zero.policy = T)

results<-as.data.frame(mod.gwr3$SDF)
head(results)

#attach coefficients to original dataframe
csa.data.plot <- data.frame(csa.data[,1])

csa.data.plot$coeftotalincidents<-results$totalincidents
csa.data.plot$coefnarc12 <-results$narc12
csa.data.plot$coefprop.vacant<-results$prop.vacant
csa.data.plot$coefracdiv10<-results$racdiv10
csa.data.plot$coeffemhhs10 <-results$femhhs10
csa.data.plot$coefabsmd14 <-results$absmd14
csa.data.plot$coefsusp13 <-results$susp13
csa.data.plot$coefliquor14 <-results$liquor14
csa.data.plot$coefheatgas14 <-results$heatgas14
csa.data.plot$coefelheat14 <-results$elheat14
csa.data.plot$coefwlksc11 <-results$wlksc11


#read in the shapefile using the maptools function readShapePoly

#fortify for use in ggpplot2
csa.shp2 <- spTransform(csa.shp, CRS("+proj=longlat +datum=WGS84"))
csaoutline <- tidy(csa.shp2)

#now plot the various GWR coefficients

stack(csa.data.plot, -csa) %>%
  data.frame(csa = rep(csa.data.plot$csa, 11), 
             lon = rep(coord.lon_lat$lon.med.avg, 11),
             lat = rep(coord.lon_lat$lat.med.avg, 11)) %>%
  filter(ind == "coefprop.vacant") %>%
  ggplot(aes(x = lon, y = lat)) + 
  geom_jitter(aes(), size=0.05) +
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs")) + 
  geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()


gwr.point1<- ggplot(csa.data.plot, aes(x=lon.med.avg, y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefprop.vacant)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

# gwr.point2<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
#   geom_point(aes(colour=csa.data.plot$coefcitytax.avg)) + 
#   scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
#                          midpoint  = 0, space = "rgb", na.value = "grey50", 
#                          guide = "colourbar", guide_legend(title="Coefs"))
# gwr.point2 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
#   coord_equal()

gwr.point3<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefprop.vacant)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = 0, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point3 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point4<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefracdiv10)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = 0, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point4 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point5<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coeffemhhs10)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = 0, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point5 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point6<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefabsmd14)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point6 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point7 <- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefsusp13)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point7 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point8 <- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefliquor14)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point8 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point9 <- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefheatgas14)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point9 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()

gwr.point10 <- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
  geom_point(aes(colour=csa.data.plot$coefwlksc11)) + 
  scale_colour_gradient2(low = "red", mid = "green", high = "blue", 
                         midpoint  = -0.4, space = "rgb", na.value = "grey50", 
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point10 + geom_path(data=csaoutline,aes(long, lat, group=id), colour="grey") + 
  coord_equal()


## Residuals

res <- mod.gwr$SDF$gwr.e

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=quantile(res, probs = seq(0, 1, 0.25)), rtimes = 1)
# classIntervals(res, n=5, style="fixed", fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
# cut(res,quantile(res, probs = seq(0, 1, 0.25)))
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuals from GWR Model",ncol=5)

## Residual Autocorrelation

moran.test(res, listw=W_cont_el_mat, zero.policy=T)


## Coefficients

coef <- mod.gwr$SDF$pcincome

classes_fx <- classIntervals(coef, n=5, style="fixed", fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey",pretty=T)
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Local Coefficient Estimates (per capita income)",ncol=3)

detach()
unlink("analyses_data.RData")
setwd("..")