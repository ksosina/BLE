

## Load spatial packages

# library(maps)         ## Projections
# library(maptools)     ## Data management
library(sp)           ## Data management
library(dplyr)        ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
# library(splancs)      ## Kernel Density
# library(spatstat)     ## Geostatistics
# library(pgirmess)     ## Spatial autocorrelation
# library(RColorBrewer) ## Visualization
# library(classInt)     ## Class intervals
library(spgwr)        ## GWR
library(ggplot2)      ## Plotting
library(broom)        ## Data management
library(GWmodel)      ## Predict GWR
library(ModelMap)     ## Random Forest and CV splitting



setwd("Data")
names(csa.data.anal) <- gsub("[[:digit:]]", "", names(csa.data.anal))
names(csa.data.anal_13) <- gsub("[[:digit:]]", "", names(csa.data.anal_13))
names(csa.data.anal_12) <- gsub("[[:digit:]]", "", names(csa.data.anal_12))
names(csa.data.anal_11) <- gsub("[[:digit:]]", "", names(csa.data.anal_11))
names(block_data.anal) <- gsub("[[:digit:]]", "", names(block_data.anal))
names(block_data.anal_11) <- gsub("[[:digit:]]", "", names(block_data.anal_11))
names(block_data.anal_12) <- gsub("[[:digit:]]", "", names(block_data.anal_12))
names(block_data.anal_13) <- gsub("[[:digit:]]", "", names(block_data.anal_13))

BG_neighbhd_csa -> BG_neighbhd

names(BG_neighbhd_csa) <- tolower(names(BG_neighbhd_csa))
BG_neighbhd_csa[,c(2,3,6,9:13,15)] -> my_data
my_data <- data.frame(my_data[,1:2],apply(my_data[,-c(1:2, 6:7)], 2, scale), my_data[,6:7])
my_data %>%
  inner_join(block_data.anal[,-c(4,5,15:16)]) -> my_block


BG_neighbhd_csa[,c(2,3,6,9:13,15)] -> my_data

my_data %>%
  inner_join(block_data.anal[,c(1:3)]) %>%
  select(-neighborhood, -block) %>% 
  group_by(csa) %>%
  summarise_all(mean) -> my_data

my_data <- data.frame(my_data[,1],apply(my_data[,-c(1, 5:6)], 2, scale), my_data[,5:6])

my_data %>% 
  inner_join(csa.data.anal[,-c(4:6, 27:28)])-> my_csa

rm(my_data)


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



# col.lm <- lm(lifeexp~.,data=csa.data.anal[,-c(1,27,28)])
# 
# col.glm <- glm(lifeexp~.,data=csa.data.anal[,-c(1,27,28)], family = Gamma(link = log))
# 
# col.glm2 <- glm(lifeexp~.,data=csa.data.anal[,-c(1,27,28)], family = Gamma)

col.lm <- lm(lifeexp~.,data=my_csa[,-c(1,7,8)])

col.glm <- glm(lifeexp~.,data=my_csa[,-c(1,7,8)], family = Gamma(link = log))

col.glm2 <- glm(lifeexp~.,data=my_csa[,-c(1,7,8)], family = Gamma)

# step <- stepAIC(col.lm, direction="both")
st1 <- step(col.lm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.lm),
            trace = 0)
st2 <- step(col.glm, 
            scope = list(lower = lifeexp ~ propfemhh + totalincidents + prop.vacant, upper = col.glm),
            trace = 0)
st3 <- step(col.glm2, 
            scope = list(lower = lifeexp ~ totalincidents + prop.vacant, upper = col.glm2),
            trace = 0)


summary(st1);summary(st2);summary(st3)
col.lm <- lm(scale(lifeexp, scale = F) ~ racdiv + femhhs + totalincidents + prop.vacant + narc + 
               absmd + susp + liquor + heatgas + elheat + wlksc, data = csa.data.anal)

col.lm <- lm(scale(lifeexp, scale = F) ~ propfemhh + totalincidents + prop.vacant + 
               abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
               narc, data = my_csa)


summary(col.lm)
plot(col.lm, which=3)

resids<-residuals(col.lm)
colours <- c("dark blue", "blue", "red", "dark red") 
#here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe

map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(coord.lon_lat[,2],coord.lon_lat[,3])) 

## Contiguity Neighbors

csa.shp <-  rgdal::readOGR(file.path("wip", "health"), "health") 
coord.shp <- coordinates(csa.shp)

W_cont_el <- poly2nb(csa.shp, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)
print(W_cont_el_mat, zero.policy=TRUE) 
moran.test(resids,W_cont_el_mat, zero.policy = T)

## Non-Contiguity Neighbors (snap distance = 100,000km)

W_cont_s <- poly2nb(csa.shp, queen=F, snap= km2d(100000))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)
print(W_cont_s_mat, zero.policy=TRUE) 
moran.test(resids,listw = W_cont_s_mat, zero.policy = T)


#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your CSA dataframe and plot using geom_point in ggplot2
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 


## Autoregressive lag model
#Use formula(st3)
row.names(csa.data.anal) <- attr(W_cont_s_mat, "region.id")
row.names(csa.data.anal_11) <- attr(W_cont_s_mat, "region.id")
row.names(csa.data.anal_12) <- attr(W_cont_s_mat, "region.id")
row.names(csa.data.anal_13) <- attr(W_cont_s_mat, "region.id")

lg.fit <- lagsarlm(formula = scale(lifeexp, scale = F) ~ racdiv + femhhs + totalincidents + 
                     prop.vacant + narc + absmd + susp + liquor + heatgas + elheat + 
                     wlksc, data = csa.data.anal, listw = W_cont_el_mat, zero.policy = T)
m <- mean(csa.data.anal$lifeexp)

lg.fit2 <- lagsarlm(formula = scale(lifeexp, scale = F) ~ racdiv + femhhs + totalincidents + 
                      prop.vacant + narc + absmd + susp + liquor + heatgas + elheat + 
                      wlksc, data = csa.data.anal, listw = W_cont_s_mat)

## GWR

#Convert to spdataframe
coordinates(csa.data.anal) <- ~lon.med.avg + lat.med.avg
coordinates(csa.data.anal_11) <- ~lon.med.avg + lat.med.avg
coordinates(csa.data.anal_12) <- ~lon.med.avg + lat.med.avg
coordinates(csa.data.anal_13) <- ~lon.med.avg + lat.med.avg
coordinates(block_data.anal) <- ~lon.med + lat.med
coordinates(block_data.anal_11) <- ~lon.med + lat.med
coordinates(block_data.anal_12) <- ~lon.med + lat.med
coordinates(block_data.anal_13) <- ~lon.med + lat.med
coordinates(my_csa) <- ~lon + lat


# st4 <- model.selection.gwr(DeVar = "lifeexp", InDeVars = names(csa.data.anal[,-c(1,27,28)])[-1],
#                     data=csa.data.anal[,-c(1,27,28)],
#                     bw = 3,
#                     adaptive = T,
#                     kernel = "gaussian",
#                     longlat = T)
# model.view.gwr(DeVar = "lifeexp", InDeVars = names(csa.data.anal[,-c(1,27,28)])[-1], model.list = st4[[1]])
# 
# st5 <- model.selection.gwr(DeVar = "lifeexp", InDeVars = names(csa.data.anal[,-c(1,27,28)])[-1],
#                            data=csa.data.anal_11[,-c(1,27,28)],
#                            bw = 3,
#                            adaptive = T,
#                            kernel = "gaussian",
#                            longlat = T)
# st6 <- model.selection.gwr(DeVar = "lifeexp", InDeVars = names(csa.data.anal[,-c(1,27,28)])[-1],
#                            data=csa.data.anal_12[,-c(1,27,28)],
#                            bw = 3,
#                            adaptive = T,
#                            kernel = "gaussian",
#                            longlat = T)
# st7 <- model.selection.gwr(DeVar = "lifeexp", InDeVars = names(csa.data.anal[,-c(1,27,28)])[-1],
#                            data=csa.data.anal_13[,-c(1,27,28)],
#                            bw = 3,
#                            adaptive = T,
#                            kernel = "gaussian",
#                            longlat = T)
# # st4[[1]] %in% st5[[1]] %in% st6[[1]] %in% st7[[1]]
# 
# com <- Reduce(intersect, list(st4[[1]], st5[[1]], st6[[1]], st7[[1]]))
# st4[[2]][st4[[1]] %in% com,]
# model.sort.gwr(st4,numVars = 24, ruler.vector = c(1:300))

# gwr.bisquare
# gwr.tricube
# m <- mean(csa.data.anal$lifeexp14)
# csa.data.anal$lifeexp14.cs <- scale((csa.data.anal$lifeexp14), scale = T)
# CwG <- gwr.sel(formula(st3), 
#                data=csa.data.anal,
#                # coords = coord.lon_lat[,-1], 
#                longlat = T, adapt = T,
#                gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr <- gwr(formula(st3), 
#                data=csa.data.anal, 
#                # coords = coord.shp,
#                longlat = T,
#                adapt=CwG, 
#                gweight=gwr.Gauss,
#                hatmatrix = T)
# mod.gwr
# 
# CwG1 <- gwr.sel(formula(st3), 
#                 data=csa.data.anal,
#                # coords = coord.lon_lat[,-1], 
#                longlat = T, adapt = F,
#                gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr1 <- gwr(formula(st3), 
#                 data=csa.data.anal, 
#                # coords = coord.shp,
#                longlat = T,
#                bandwidth=CwG1, 
#                gweight=gwr.Gauss,
#                hatmatrix = T,
#                predictions = T)
# mod.gwr1
# 
# CwG2 <- ggwr.sel(formula(st3), 
#                 data=csa.data.anal,
#                 # coords = coord.lon_lat[,-1], 
#                 longlat = T, adapt = F, family = Gamma,
#                 gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr2 <- ggwr(formula(st3), 
#                 data=csa.data.anal, 
#                 # coords = coord.shp,
#                 longlat = T,
#                 bandwidth=CwG2, 
#                 gweight=gwr.Gauss,family = Gamma)
# mod.gwr2

# CwG3 <- ggwr.sel(formula(st3), 
#                  data=csa.data.anal,
#                  # coords = coord.lon_lat[,-1], 
#                  longlat = T, adapt = F, family = Gamma(link = log),
#                  gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr3 <- ggwr(formula(st3), 
#                  data=csa.data.anal, 
#                  # coords = coord.shp,
#                  longlat = T,
#                  bandwidth=CwG3, 
#                  gweight=gwr.Gauss,family = Gamma(link = log))
# mod.gwr3

CwG3 <- ggwr.sel(formula(st3), 
                 data=my_csa,
                 # coords = coord.lon_lat[,-1], 
                 longlat = T, adapt = F, family = Gamma(link = log),
                 gweight=gwr.Gauss, verbose=F)

mod.gwr3 <- ggwr(formula(st3), 
                 data=my_csa, 
                 # coords = coord.shp,
                 longlat = T,
                 bandwidth=CwG3, 
                 gweight=gwr.Gauss,family = Gamma(link = log))
mod.gwr3

# CwG4 <- ggwr.sel(formula(st4[[1]][[300]][[1]]), 
#                  data=csa.data.anal,
#                  # coords = coord.lon_lat[,-1], 
#                  longlat = T, adapt = F, family = Gamma(link = log),
#                  gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr4 <- ggwr(formula(st4[[1]][[300]][[1]]), 
#                  data=csa.data.anal, 
#                  # coords = coord.shp,
#                  longlat = T,
#                  bandwidth=3, 
#                  gweight=gwr.Gauss,family = Gamma(link = log))
# mod.gwr4

# CwG4 <- ggwr.sel(lifeexp14~ racdiv10 + femhhs10 +
#                    totalincidents + prop.vacant, 
#                  data=csa.data.anal,
#                  # coords = coord.lon_lat[,-1], 
#                  longlat = T, adapt = F, family = gaussian(link = log),
#                  gweight=gwr.Gauss, verbose=F)
# 
# mod.gwr4 <- ggwr(lifeexp14~ racdiv10 + femhhs10 +
#                    totalincidents + prop.vacant, 
#                  data=csa.data.anal, 
#                  # coords = coord.shp,
#                  longlat = T,
#                  bandwidth=CwG3, 
#                  gweight=gwr.Gauss,family = gaussian(link = log))
# mod.gwr4

# my.link <- function()
# {
#   linkfun <- function(mu) mu 
#   linkinv <- function(eta) eta
#   mu.eta <- function(eta) eta + 
#   valideta <- function(eta) TRUE
#   link <- paste0("logexp(", days, ")")
#   structure(list(linkfun = linkfun, linkinv = linkinv,
#                  mu.eta = mu.eta, valideta = valideta, name = link),
#             class = "link-glm")
# }


## Check dist for the predictions under each model...mod.gwr3 is the best

# sqrt(sum((mod.gwr$lm$fitted.values - mod.gwr2$lm$y)^2))
# sqrt(sum((mod.gwr1$lm$fitted.values - mod.gwr2$lm$y)^2))
# sqrt(sum((mod.gwr2$lm$fitted.values - mod.gwr2$lm$y)^2))
# sqrt(sum((mod.gwr3$lm$fitted.values - mod.gwr2$lm$y)^2))

# plot(mod.gwr$lm$y, type = "l", col = "red")
# lines(mod.gwr$SDF$pred, col = "blue")
# lines(col.lm$fitted.values, col = "black")
# 
# plot(mod.gwr2$lm$y, type = "l", col = "red")
# lines(mod.gwr2$lm$fitted.values, col = "blue") ##bad
# lines(mod.gwr3$lm$fitted.values, col = "black") ##Good
# lines(mod.gwr4$lm$fitted.values, col = "black") ##bad


# plot(mod.gwr3$lm$y, type = "l", col = "red")
# lines(lg.fit$fitted.values + m, col = "blue") 
# lines(lg.fit2$fitted.values + m, col = "navyblue") 
# lines(mod.gwr3$lm$fitted.values, col = "black") 
# lines(col.lm$fitted.values + m, col = "darkgreen") 
# lines(health.sub$LifeExp11, col = "navyblue")
# lines(health.sub$LifeExp12, col = "orange")
# lines(health.sub$LifeExp13, col = "brown")
# lines(health.sub$LifeExp14)

#Prediction

# bw <- bw.gwr(formula(st4[[1]][[300]][[1]]),data = csa.data.anal, approach="CV",kernel="gaussian",
#        adaptive=F, p=2, theta=0, longlat=T)

# bw <- bw.gwr(scale(lifeexp,scale = F) ~ racdiv + mhhi + femhhs + citytax.avg + statetax.avg + 
#                narc + absmd + liquor + heatgas + elheat + wlksc,data = csa.data.anal, approach="CV",kernel="gaussian",
#              adaptive=F, p=2, theta=0, longlat=T)

pred <- predict.sarlm(lg.fit2, newdata = csa.data.anal_13, listw = W_cont_s_mat)
# pred.t <- gwr.predict(formula(st4[[1]][[300]][[1]]),data = csa.data.anal, predictdata = csa.data.anal_11,
#             bw = bw, kernel = "gaussian",adaptive = F, longlat = T)
# pred.t <- gwr.predict(scale(lifeexp,scale = F) ~ racdiv + mhhi + femhhs + citytax.avg + statetax.avg + 
#                         narc + absmd + liquor + heatgas + elheat + wlksc,data = csa.data.anal, predictdata = csa.data.anal_11,
#                       bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

bw <- bw.gwr(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
               abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
               narc,data = my_csa, approach="CV",kernel="gaussian",
             adaptive=F, p=2, theta=0, longlat=T)
pred.t <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
                        abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
                        narc,data = my_csa, 
                      # predictdata = csa.data.anal_11,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

# bw <- bw.gwr(totalincidents ~ 1,data = block_data.anal, approach="CV",kernel="gaussian",
#              adaptive=F, p=2, theta=0, longlat=T)

my_csa %>% data.frame() %>%
  subset(select = c(csa,abshs) ) %>%
  inner_join(my_block) -> my_block
my_block$totalincidents[is.nan(my_block$totalincidents)] <- 0
coordinates(my_block) <- ~lon + lat
pred.b <- gwr.predict(scale(lifeexp,scale = F) ~ propfemhh + totalincidents + prop.vacant + 
                        abshs + susp + liquor + heatgas + elheat + wlksc + racdiv + 
                        narc,data = my_csa, 
                      predictdata = my_block,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)
data.frame(my_block, pred = pred.b$SDF$prediction + 73.76) -> my_block
my_block %>%
  group_by(csa) %>%
  summarise(pred = mean(pred.b.SDF.prediction)) -> csa_pred
pred.b <- gwr.predict(formula(st3),data = csa.data.anal, predictdata = block_data.anal_11,
                      bw = bw, kernel = "gaussian",adaptive = F, longlat = T)

block_data.anal_11@data$pred <- pred.b$SDF$prediction

data.frame(block_data.anal, pred = pred.b$SDF$prediction) %>%
  group_by(csa) %>%
  summarise(pred = mean(pred, na.rm = T)) %>%
  inner_join(csa.data.anal_13@data[,1:2]) -> test

plot(test$lifeexp, col = "red",type = "l")
lines(test$pred)
# row.names(csa.data.anal_11) <- attr(W_cont_s_mat, "region.id")

plot(my_csa$lifeexp, type = "l", col = "red")
# lines(as.matrix(pred) + m, col = "blue") 
lines(pred.t$SDF$prediction + 73.76 )
lines(lg.fit2$fitted.values + m, col = "navyblue")

plot(my_csa$lifeexp, type = "l", col = "red")
lines(csa_pred$pred, col = "blue")
lines(pred.t$SDF$prediction + 73.76 )

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

detach("package:dplyr", unload = T);detach("package:spgwr", unload = T)
setwd("..")