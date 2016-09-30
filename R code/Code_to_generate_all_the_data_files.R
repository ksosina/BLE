#Create all data files to be used during analysis
setwd(file.path("Data"))

#First dataset is Census. 1. 2010-2014 has missing info for 2010, 2012, and 2013
census10.14 <- readr::read_csv(file.path("raw_data", "census.csv"))
census10 <- readr::read_csv(file.path("raw_data", "census10.csv"))
census10.12 <- readr::read_csv(file.path("raw_data", "census12.csv"))
census10.13 <- readr::read_csv(file.path("raw_data", "census13.csv"))

#First rename colums to match
names(census10)[c(1:2,5:6, 8:10,12,13:14, 16:17)] <- c(names(census10.14)[c(1:2,5:7,9,8,10,13:16)])
names(census10)[15] <- "age44_10"
names(census10.13)[1] <- names(census10.14)[1]

#Second merging datasets
library(dplyr)

#I noticed that some variables have * in front of each figure for hhs10 and mhhi13 in census10.13 so I will replace em
census10.13 %>% arrange_(.dots=names(census10.13)[1:20]) %>%
  mutate(hhs10 = as.numeric(gsub("\\*|\\* |,", "", hhs10)),
         mhhi13 = as.numeric(gsub("\\*|\\* |,", "", mhhi13))) -> census10.13

#I noticed that mhhi12 has $ in front of each figure in census10.12 so I will replace em
census10.12  %>% arrange_(.dots=names(census10.12)[1:20]) %>%
  mutate(mhhi12 = gsub("\\$", "", mhhi12), mhhi12 = as.numeric(mhhi12)) -> census10.12



#Check which varaibles are the same across the datasets
census10 %>% arrange_(.dots=names(census10)[1:20]) %>%
  select_(.dots = names(census10)[2:20]) %>% 
  as.data.frame() %>% apply(2, summary) -> c1
census10.12 %>% 
  select_(.dots = names(census10.12)[2:20]) %>% 
  as.data.frame() %>% apply(2, summary) -> c2
census10.13 %>% 
  select_(.dots = names(census10.13)[2:20]) %>% 
  mutate_each(funs(as.numeric)) %>% apply(2, summary)  -> c3
census10.14 %>% arrange_(.dots=names(census10.14)[1:20]) %>%
  select_(.dots = names(census10.14)[2:20][-16]) %>% apply(2, summary) -> c4

#IDs which variables are the same, note that I excluded hhs10 from both c2,and c3 since it is NA in c4
identical(c1,c2);identical(c2,c3);identical(c2[,-16],c4);identical(c3[,-16],c4)

sapply(1:19, function(x){
  identical(c1[,x],c2[,x])
})


#So we can join c2(census10.12) and c3(census10.13) on the first 20 variables c.f  names(census10.12)[names(census10.12) %in% names(census10.13)]

census12_14 <- inner_join(census10.12,census10.13) %>% 
  inner_join(census10.14, by = names(census10.14)[2:20][-16]) %>% #join on 19 variables after we exclude hhs10
  select(-CSA2010.y,-hhs10.y, CSA2010 = CSA2010.x)
rm(census10.12, census10.13, census10.14, c1,c2,c3,c4)


#### ID which neighborhoods fall into a CSA

##Load data from real properties that contains block info
data <- readr::read_csv(file.path("raw_data", "property.csv"))
dat1 <- subset(data, select = names(data)[c(2:3,6,8,9,11,13:15)])
subset(data, select = c("Block", "Neighborhood", "Location")) -> data

data <- na.omit(data)

new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)
data <- data.frame(data[,c(1,2)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))
dat1$lon <- NA;dat1$lat <- NA
dat1[!is.na(dat1$Neighborhood),] <- data.frame(dat1[!is.na(dat1$Neighborhood),][,1:9],
                                               lon = as.numeric(new_loc[,2]), 
                                               lat = as.numeric(new_loc[,1]))

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

#Check if neighborhoods are the same, they should be! (note that the lengths are diff since dat1 has NA, but no matter)
identical(sort(unique(dat1$Neighborhood)), sort(unique(neighbhd_csa$Neighborhood)))

#Careful file size for join without summary is ~2GB!!!
inner_join(neighbhd_csa, dat1) %>%
  mutate(CityTax = gsub("\\$", "", CityTax), CityTax = as.numeric(CityTax),
         StateTax = gsub("\\$", "", StateTax), StateTax = as.numeric(StateTax),
         AmountDue = gsub("\\$", "", AmountDue), AmountDue = as.numeric(AmountDue)) %>%
  group_by(CSA, Neighborhood, Block) %>%
  summarise(CityTax.med = median(CityTax, na.rm = T), StateTax.med = median(StateTax, na.rm = T), 
            AmountDue.med = median(AmountDue, na.rm = T), lon.med = median(lon, na.rm = T), lat.med = median(lat)) -> csa.prop

pryr::object_size(csa.prop)


#Outcome data


health <- readr::read_csv(file.path(".", "raw_data", "child_and_fam_wellbeing.csv"))
clnames <- names(health)
clnames[1] <- "CSA"
names(health) <- clnames


gsub("_[[:digit:]]*","",names(health))[-1] -> variables
variables[variables == "mort1"] <- "mort01"
variables <- sapply(variables, function(x){
  substr(x, start = 1, stop = nchar(x) - 2)
})
unname(variables) -> variables
unique(variables) -> var.names

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

#This will have the same number of rows as csa.prop since block neighborhood combinations are unique
inner_join(csa.prop, health.sub) -> csa.prop.health  
rm(csa, dat.le,data, new_loc, neighbhd_csa, dat1)
#Interesting not relevant yet

# # Use 'acs' package
# # Get place or county name for baltimore city 
# View(fips.place[fips.place$STATE=="MD",])
# 
# #view couty sub division for baltimore city
# View(fips.county.subdivision[fips.county.subdivision$STATE=="MD",])
# 
# #make geo location for acs.fetch
# acs::geo.make(state = "MD", 
#               county = "Baltimore city", 
#               tract = "*") -> place
# api.key.install(key ="")
# acs.fetch(2014,geography = place, 
#           keyword = "male", 
#           case.sensitive = F,
#           col.names = "pretty") ->acs.data


detach("package:dplyr", unload=TRUE)

setwd(file.path(".."))