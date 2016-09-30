setwd(file.path("Data"))

#### ID which neighborhoods fall into a CSA

##Load data from real properties that contains block info
data <- readr::read_csv(file.path("raw_data", "property.csv.gz"))
subset(data, select = c("Block", "Neighborhood", "Location")) -> data

data <- na.omit(data)

new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)
data <- data.frame(data[,c(1,2)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))
dat1 <- data

# dat <- data.frame(Neighborhood = sort(unique(data$Neighborhood)),
#                   lon = data$lon,
#                   lat =data$lat, stringsAsFactors=FALSE)

#Step 1
### Check if Lng and Lat fall inside polygons from ESRI Shape file for Child and wellbeing (this has the outcome)
dat.le <-  rgdal::readOGR(file.path("wip", "health"), "health")
csa <- as.character(dat.le$CSA2010)
dat.le <- sp::spTransform(dat.le, sp::CRS("+proj=longlat +datum=WGS84")) #SpatialPolygonsDataFrame


# Assignment modified according
sp::coordinates(data) <- ~lon + lat #SpatialPointsDataFrame

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
sp::proj4string(data) <- sp::proj4string(dat.le)

sp::over(dat.le, data, returnList = T) -> neighbhd_csa #gives which Neighborhood belongs to what CSA

names(neighbhd_csa) <- csa

#Gives a dataframe with cols CSA, Blocks, and Neighborhood so I can match block level info to CSA
neighbhd_csa <- plyr::ldply (neighbhd_csa, data.frame) 
clnames <- names(neighbhd_csa)
clnames[1] <- "CSA"
names(neighbhd_csa) <- clnames

#Got the CSA to NBHD from BNIA site to compare
csa_nsa <- readxl::read_excel(file.path(".", "raw_data", "csa_nsa.xlsx"))

#Caution CSA and neighbourhoods are not in 1-1
with(neighbhd_csa, tapply(CSA, Neighborhood, function(x) length(unique(x)))) -> test.dat

#Get neighbhds where count of CSA = 2 and check
unique(neighbhd_csa[neighbhd_csa$Neighborhood == names(test.dat[test.dat == 2][1]),][,c(1,3)]) -> not.unique
not.unique


#Step 2
# Now that all that is done, I start merging (on block and neighbhd) to get CSA level, neighborhood level and block level data in one dataset
health <- readr::read_csv(file.path(".", "raw_data", "child_and_fam_wellbeing.csv"))
clnames <- names(health)
clnames[1] <- "CSA"
names(health) <- clnames

# Get variable names
library(dplyr)

gsub("_[[:digit:]]*","",names(health))[-1] -> variables
variables[variables == "mort1"] <- "mort01"
variables <- sapply(variables, function(x){
  substr(x, start = 1, stop = nchar(x) - 2)
})
unname(variables) -> variables
unique(variables) -> var.names

# Get years 
# years <- sapply(gsub("[^_[:digit:]]","",names(health))[-1], function(x){
#   substr(x, start = nchar(x)-1, stop = nchar(x))
# }
# )
# unname(years) -> years

# subset(health, drop = grep(paste0(x), names(health), value = T) )
setdiff(names(health), grep(paste0("mort"), names(health), value = T)) ->rm.mort

#Change from short to long

health.long <- lapply(var.names[var.names!= "mort"], function(x){
  #get the columns
  columns <- grep(paste0(x),rm.mort, value = T)
  
  #get the time in years
  time <- sapply(gsub("[^_[:digit:]]","",columns), function(x){
    substr(x, start = nchar(x)-1, stop = nchar(x))
  }
  )
  unname(time) -> time
  
  #Select the columns
  subset(health, select = c("CSA",columns)) -> dat.h
  
  n <- dim(dat.h)[1]

  # print(length(time))
  dat.h <- tidyr::gather(dat.h, variable, value, -CSA)
  dat.h$time <- rep(as.numeric(time), each = n)
  dat.h
  # data.frame(tidyr::gather(dat.h, variable, value, -CSA), time = time)
  # rbind(cbind(tidyr::gather(dat.h, variable, value, -CSA), time))
  
})

plyr::ldply(health.long, data.frame) -> health.long


#get the columns
columns <-  sapply(strsplit(grep(paste0("mort"),names(health), value = T),"_"),function(x) x[1])
unique(columns) -> columns

health.long2 <- lapply(columns, function(x){
  
  #get the time in years
  time <- sapply(strsplit(grep(paste0("^", x, "_"),
                               names(health), value = T),"_"),function(x) x[2])
  time <- as.numeric(time)
  
  #Select the columns
  subset(health, select = c("CSA",
                            grep(paste0("^", x, "_"), names(health), value = T))
         ) -> dat.h
  
  n <- dim(dat.h)[1]
  
  # print(length(time))
  dat.h <- tidyr::gather(dat.h, variable, value, -CSA)
  dat.h$time <- rep(as.numeric(time), each = n)
  dat.h
  # data.frame(tidyr::gather(dat.h, variable, value, -CSA), time = time)
  # rbind(cbind(tidyr::gather(dat.h, variable, value, -CSA), time))
  
})

plyr::ldply(health.long2, data.frame) -> health.long2

health.long <- rbind(health.long, health.long2)

rm(health.long2, columns, rm.mort, var.names, variables, new_loc)

#Merge
health.sub <- subset(health, select = c("CSA", "LifeExp11", 
                                        "LifeExp12", "LifeExp13",
                                        "LifeExp14"))


inner_join(health.sub, neighbhd_csa) %>% #This will have the same number of rows as neighbhd_csa since block neighborhood combinations are unique
  inner_join(dat1) -> merged.h_n         #This will have more rows since dat1 each block in property.csv.gz has multiple streets

# Mantel's Test
#we need CSA to be unique so get the median longitude and latitude per CSA

merged.h_n %>% group_by(CSA) %>%
  summarise(lon.med = median(lon), lat.med = median(lat)) -> mtdata

#note that the number of rows for mtdata is the same as the number of CSA's in our data. Furthermore the number of CSA from BNIA is the same (Baltimore city is not a CSA!!! And so should not be used in the calculations!)

mtdata %>% inner_join(health.sub) -> mtdata

#Testing
#2011
csa.dists <- geosphere::distm(cbind(mtdata$lon.med, mtdata$lat.med), fun = distVincentyEllipsoid) #dist(cbind(mtdata$lon.med, mtdata$lat.med), method = "euclidean")
csa.dists <- as.dist(csa.dists)
le11.dists <- dist(mtdata$LifeExp11, method = "euclidean")

# ade4::mantel.rtest(csa.dists, le11.dists, nrepet = 9999)
# plot(ade4::mantel.rtest(csa.dists, le11.dists, nrepet = 9999))
ade4::mantel.randtest(csa.dists, le11.dists, nrepet = 9999)
plot(ade4::mantel.randtest(csa.dists, le11.dists, nrepet = 9999))

#2012
le12.dists <- dist(mtdata$LifeExp12, method = "euclidean")


ade4::mantel.randtest(csa.dists, le12.dists, nrepet = 9999)
plot(ade4::mantel.randtest(csa.dists, le12.dists, nrepet = 9999))

#2013
le13.dists <- dist(mtdata$LifeExp13, method = "euclidean")


ade4::mantel.randtest(csa.dists, le13.dists, nrepet = 9999)
plot(ade4::mantel.randtest(csa.dists, le13.dists, nrepet = 9999))

#2014
le14.dists <- dist(mtdata$LifeExp14, method = "euclidean")


ade4::mantel.randtest(csa.dists, le14.dists, nrepet = 9999)
plot(ade4::mantel.randtest(csa.dists, le14.dists, nrepet = 9999))


#Moran's I
csa.dists <- as.matrix(csa.dists)

csa.dists.inv <- 1/csa.dists #solve(csa.dists)
diag(csa.dists.inv) <- 0

#2011
ape::Moran.I(mtdata$LifeExp11, csa.dists.inv)

#2012
ape::Moran.I(mtdata$LifeExp12, csa.dists.inv)

#2013
ape::Moran.I(mtdata$LifeExp13, csa.dists.inv)

#2014
ape::Moran.I(mtdata$LifeExp14, csa.dists.inv)

detach("package:dplyr", unload=TRUE)
# do.call(rbind.data.frame, neighbhd_csa) -> df

setwd(file.path(".."))