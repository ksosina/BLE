#Plotting 

setwd(file.path("..", "Data"))  

n <- length(list.files(file.path("..", "Plots")))
if (n == 0){
  packages <- c("ggplot2","lubridate", "downloader", 
                "readr", "readxl", "maptools", "RColorBrewer", 
                "ggmap", "devtools", "rgeos", "broom", "rgdal")
  sapply(packages, library, character.only = T, quietly = T)
  
  
  map <- get_map(location = 'Baltimore', zoom = 11)
  p <- ggmap(map)
  
  data <- read_csv(file.path("raw_data", "property.csv.gz"))
  subset(data, select = c("Block", "Neighborhood", "Location")) -> data
  data <- na.omit(data)
  new_loc <-  sapply(data$Location, function(x) {
    y <- substr(x, start = 2, stop = nchar(x) - 1)
    strsplit(y, ", ")[[1]]
  }
  )
  new_loc <- t(new_loc)
  data <- data.frame(data[,c(1,2)], lat = as.numeric(new_loc[,1]), lon = as.numeric(new_loc[,2]))
  
  
  
  p + geom_point(data=data,aes(x = lon, y = lat, colour = as.factor(data$Block)))+
    theme(legend.position = "none") 
  
  dat <- data.frame(Neighborhood = sort(unique(data$Neighborhood)),
                    lon = tapply(data$lon, data$Neighborhood, mean),
                    lat =tapply(data$lat, data$Neighborhood, mean), stringsAsFactors=FALSE)
  dat[nchar(dat$Neighborhood) <= 7,] -> dat1
  map <- get_map(location = "Baltimore City", zoom = 12, maptype = "roadmap" )
  p <- ggmap(map)
  p + geom_point(data=data,aes(x = lon, y = lat, colour = as.factor(data$Neighborhood) )) +
    geom_text(data=dat1,
              aes(label = as.factor(dat1$Neighborhood)), 
              colour="Black",size=2,hjust="center", 
              vjust="center") +
    theme(legend.position = "none") 
  # detach(real_prop_block)
  
  
  
  #Load in the data file (could this be done from the downloaded zip file directly?
  data <- read_csv(file.path("raw_data", "property.csv.gz"))
  subset(data, select = c("Block", "Neighborhood", "Location")) -> data
  data <- na.omit(data)
  new_loc <-  sapply(data$Location, function(x) {
    y <- substr(x, start = 2, stop = nchar(x) - 1)
    strsplit(y, ", ")[[1]]
  }
  )
  new_loc <- t(new_loc)
  data <- data.frame(data[,c(1,2)], lat = as.numeric(new_loc[,1]), lon = as.numeric(new_loc[,2]))
  
  dat <- data.frame(Neighborhood = sort(unique(data$Neighborhood)),
                    lon = tapply(data$lon, data$Neighborhood, median),
                    lat =tapply(data$lat, data$Neighborhood, median), stringsAsFactors=FALSE)
  
  ##Choose length of character the name neighbourhood has to have to be displayed
  n.name <- 7
  dat[nchar(dat$Neighborhood) <= n.name,] -> dat1
  map <- get_map(location = "Baltimore City", zoom = 12, maptype = "roadmap" )
  p <- ggmap(map)
  p + geom_point(data=data,aes(x = lon, y = lat, colour = factor(data$Neighborhood):factor(data$Block) )) +
    geom_text(data=dat1,
              aes(label = as.factor(dat1$Neighborhood)), 
              colour="Black",size=2,hjust="center", 
              vjust="center") +
    labs(title = "Neighbourhoods as defined by Blocks Using just Block level data",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "none") 
  
  ggsave(filename = file.path("..", "Plots", 
                              "n_block_block.png"), width = 45, height = 45, units = "cm")
  
  #Fit of neighbourhood info
  gor <- readOGR(file.path("wip", "nhood"), "nhood_2010")
  gor <- spTransform(gor, CRS("+proj=longlat +datum=WGS84"))
  gor <- tidy(gor)
  
  p + 
    geom_point(data=data,aes(x = lon, y = lat, colour = as.factor(data$Neighborhood) ))  +
    geom_text(data = dat1, aes(label = as.factor(dat1$Neighborhood)), 
              colour="Black",size=2,hjust="center", 
              vjust="center") +
    geom_polygon(data=gor, aes(x=long, y=lat, group=group), color="red", alpha=0) +
    # geom_map(map=gor, data=gor, aes(map_id=id, x=long, y=lat, group=group), color="red", alpha=0) +
    coord_quickmap() +
    labs(title = "The fit of neighbourhood info on block data in Baltimore City",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "none") 
  
  ggsave(filename = file.path("..", "Plots", 
                              "block_block.png"), width = 45, height = 45, units = "cm")
  
  
  ##    The fit of Block info on Neighbourhood data in Baltimore City
  gor <- readOGR(file.path("wip", "blk2010"), "blk2010")
  gor <- spTransform(gor, CRS("+proj=longlat +datum=WGS84"))
  gor <- tidy(gor)
  
  
  p + 
    # geom_point(data=dat1,aes(x = lon, y = lat, colour = as.factor(dat1$Neighborhood),fill = as.factor(dat1$Neighborhood) )) +
    geom_text(data = dat1, aes(label = as.factor(dat1$Neighborhood)), 
              colour="Black",size=2,hjust="center", 
              vjust="center") +
    geom_polygon(data=gor, aes(x=long, y=lat, group=group, fill = group), color="red", alpha=0) +
    # geom_map(map=gor, data=gor, aes(map_id=id, x=long, y=lat, group=group), color="red", alpha=0) +
    coord_map() +
    labs(title = "Blocks in Baltimore City",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "none") 
  
  ggsave(filename = file.path("..", "Plots", 
                              "block_n.png"),width = 45, height = 45, units = "cm")
  
  p + 
    geom_point(data=data,aes(x = lon, y = lat, colour = as.factor(data$Neighborhood) ))  +
    geom_text(data = dat1, aes(label = as.factor(dat1$Neighborhood)), 
              colour="Black",size=2,hjust="center", 
              vjust="center") +
    geom_polygon(data=gor, aes(x=long, y=lat, group=group), color="red", alpha=0) +
    # geom_map(map=gor, data=gor, aes(map_id=id, x=long, y=lat, group=group), color="red", alpha=0) +
    coord_quickmap() +
    labs(title = "The fit of Block info on block data in Baltimore City",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "none") 
  
  ggsave(filename = file.path("..", "Plots", 
                              "block_b.png"), 
         width = 45, height = 45, units = "cm")
  
  
  unlink(file.path("wip", "blk2010"), force = T, recursive = T)
}
