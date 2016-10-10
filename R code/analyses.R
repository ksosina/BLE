

## Load spatial packages

library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(dplyr)        ## Data management
library(spdep)        ## Spatial autocorrelation
# library(gstat)        ## Geostatistics
# library(splancs)      ## Kernel Density
# library(spatstat)     ## Geostatistics
# library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR
library(ggplot2)      ## Plotting
library(broom)        ## Data management
library(MASS)         ## Model Fit



setwd("Data")

#Co-ordinates
csa.prop.health %>% 
  group_by(csa) %>%
  summarise(lon.med.avg = mean(lon.med), lat.med.avg = mean(lat.med)) -> coord.lon_lat

# coordinates(coord.lon_lat) <- ~lon.med.avg + lat.med.avg
csa.shp <-  rgdal::readOGR(file.path("wip", "health"), "health") 
coord.shp <- coordinates(csa.shp)

# rm(csa.shp)

########
## GWR (WARNING: This takes a while to run)
########

#### Distance Conversion         ####
#####################################


## Function: Convert km to degrees
km2d <- function(km){
  out <- (km/1.852)/60
  return(out)
}
# km2d(500) ## 500 km



csa.data.anal <- data.frame(csa.data[,1:2], X, coord.lon_lat[,c(2:3)])

# CwG <- gwr.sel(lifeexp14 ~ totalincidents + citytax.avg + prop.vacant, 
#                data = csa.data.anal, 
#                coords = coord.shp,
#                gweight=gwr.Gauss, verbose=F)

col.lm <- lm(lifeexp14~.,data=csa.data.anal[,-c(1,27,28)])

col.glm <- glm(lifeexp14~.,data=csa.data.anal[,-c(1,27,28)], family = Gamma(link = log))

col.glm2 <- glm(lifeexp14~.,data=csa.data.anal[,-c(1,27,28)], family = Gamma)

step <- stepAIC(col.lm, direction="both")
st1 <- step(col.lm, scope = list(lower = lifeexp14 ~ totalincidents + prop.vacant, upper = col.lm), trace = 0)
st2 <- step(col.glm, scope = list(lower = lifeexp14 ~ totalincidents + prop.vacant, upper = col.glm), trace = 0)
st3 <- step(col.glm2, scope = list(lower = lifeexp14 ~ totalincidents + prop.vacant, upper = col.glm2), trace = 0)

# col.lm <- lm(lifeexp14 ~ totalincidents + citytax.avg + prop.vacant, 
#              data = csa.data.anal)

summary(col.lm)
plot(col.lm, which=3)

resids<-residuals(col.lm)
colours <- c("dark blue", "blue", "red", "dark red") 
#here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe

map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(coord.lon_lat[,2],coord.lon_lat[,3])) 

## Contiguity Neighbors

W_cont_el <- poly2nb(csa.shp, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)
print(W_cont_el_mat, zero.policy=TRUE) 
moran.test(resids,W_cont_el_mat, zero.policy = T)

## Contiguity Neighbors (snap distance = 500km)

W_cont_s <- poly2nb(csa.shp, queen=T, snap= km2d(500))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)
print(W_cont_s_mat, zero.policy=TRUE) 
moran.test(resids,listw = W_cont_s_mat, zero.policy = T)


#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your CSA dataframe and plot using geom_point in ggplot2
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 


coordinates(csa.data.anal) <- ~lon.med.avg + lat.med.avg

# gwr.bisquare
# gwr.tricube
m <- mean(csa.data.anal$lifeexp14)
csa.data.anal$lifeexp14.cs <- scale((csa.data.anal$lifeexp14), scale = T)
CwG <- gwr.sel(formula(st3), 
               data=csa.data.anal,
               # coords = coord.lon_lat[,-1], 
               longlat = T, adapt = T,
               gweight=gwr.Gauss, verbose=F)

mod.gwr <- gwr(formula(st3), 
               data=csa.data.anal, 
               # coords = coord.shp,
               longlat = T,
               adapt=CwG, 
               gweight=gwr.Gauss,
               hatmatrix = T)
mod.gwr

CwG1 <- gwr.sel(formula(st3), 
                data=csa.data.anal,
               # coords = coord.lon_lat[,-1], 
               longlat = T, adapt = F,
               gweight=gwr.Gauss, verbose=F)

mod.gwr1 <- gwr(formula(st3), 
                data=csa.data.anal, 
               # coords = coord.shp,
               longlat = T,
               bandwidth=CwG1, 
               gweight=gwr.Gauss,
               hatmatrix = T,
               predictions = T)
mod.gwr1

CwG2 <- ggwr.sel(formula(st3), 
                data=csa.data.anal,
                # coords = coord.lon_lat[,-1], 
                longlat = T, adapt = F, family = Gamma,
                gweight=gwr.Gauss, verbose=F)

mod.gwr2 <- ggwr(formula(st3), 
                data=csa.data.anal, 
                # coords = coord.shp,
                longlat = T,
                bandwidth=CwG2, 
                gweight=gwr.Gauss,family = Gamma)
mod.gwr2

CwG3 <- ggwr.sel(formula(st3), 
                 data=csa.data.anal,
                 # coords = coord.lon_lat[,-1], 
                 longlat = T, adapt = F, family = Gamma(link = log),
                 gweight=gwr.Gauss, verbose=F)

mod.gwr3 <- ggwr(formula(st3), 
                 data=csa.data.anal, 
                 # coords = coord.shp,
                 longlat = T,
                 bandwidth=CwG3, 
                 gweight=gwr.Gauss,family = Gamma(link = log))
mod.gwr3

CwG4 <- ggwr.sel(lifeexp14~ racdiv10 + femhhs10 +
                   totalincidents + prop.vacant, 
                 data=csa.data.anal,
                 # coords = coord.lon_lat[,-1], 
                 longlat = T, adapt = F, family = gaussian(link = log),
                 gweight=gwr.Gauss, verbose=F)

mod.gwr4 <- ggwr(lifeexp14~ racdiv10 + femhhs10 +
                   totalincidents + prop.vacant, 
                 data=csa.data.anal, 
                 # coords = coord.shp,
                 longlat = T,
                 bandwidth=CwG3, 
                 gweight=gwr.Gauss,family = gaussian(link = log))
mod.gwr4

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


norm(cbind(mod.gwr$lm$fitted.values,mod.gwr$lm$y), "F")
norm(cbind(mod.gwr$SDF$pred,mod.gwr$lm$y), "F")
# cbind(mod.gwr1$lm$fitted.values,mod.gwr1$lm$y)
norm(cbind(mod.gwr1$SDF$pred,mod.gwr1$lm$y), "F")
norm(cbind(mod.gwr2$lm$fitted.values,mod.gwr2$lm$y), "F")
norm(cbind(mod.gwr3$lm$fitted.values,mod.gwr2$lm$y), "F")

plot(mod.gwr$lm$y, type = "l", col = "red")
lines(mod.gwr$SDF$pred, col = "blue")
lines(col.lm$fitted.values, col = "black")

plot(mod.gwr2$lm$y, type = "l", col = "red")
lines(mod.gwr2$lm$fitted.values, col = "blue") ##bad
lines(mod.gwr3$lm$fitted.values, col = "black") ##Good
lines(mod.gwr4$lm$fitted.values, col = "black") ##bad

gwr.morantest(mod.gwr, W_cont_el_mat, zero.policy = T)
gwr.morantest(mod.gwr1, W_cont_el_mat, zero.policy = T)
moran.test(mod.gwr1$SDF$gwr.e, W_cont_el_mat, zero.policy = T)
moran.test(mod.gwr$SDF$gwr.e, W_cont_el_mat, zero.policy = T)

results<-as.data.frame(mod.gwr$SDF)
head(results)

#attach coefficients to original dataframe
csa.data.plot <- data.frame(csa.data[,1:2], X, coord.lon_lat[,c(2:3)])

csa.data.plot$coeftotalincidents<-results$totalincidents
# csa.data.plot$coefcitytax.avg <-results$citytax.avg
csa.data.plot$coefprop.vacant<-results$prop.vacant
csa.data.plot$coefracdiv10<-results$racdiv10
csa.data.plot$coeffemhhs10 <-results$femhhs10


#read in the shapefile using the maptools function readShapePoly

#fortify for use in ggpplot2
csa.shp2 <- spTransform(csa.shp, CRS("+proj=longlat +datum=WGS84"))
csaoutline <- tidy(csa.shp2)

#now plot the various GWR coefficients

gwr.point1<- ggplot(csa.data.plot, aes(x=lon.med.avg ,y=lat.med.avg)) +
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