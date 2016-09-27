setwd(file.path("Data"))

#### ID which neighborhoods fall into a CSA

##Load data from real properties that contains block info
data <- readr::read_csv(file.path("raw_data", "property.csv"))
subset(data, select = c("Block", "Neighborhood", "Location")) -> data
data <- na.omit(data)
new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)
data <- data.frame(data[,c(1,2)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))

dat <- data.frame(Neighborhood = sort(unique(data$Neighborhood)),
                  lon = data$lon,
                  lat =data$lat, stringsAsFactors=FALSE)

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

# do.call(rbind.data.frame, neighbhd_csa) -> df

setwd(file.path(".."))