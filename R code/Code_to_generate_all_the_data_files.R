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
library(dplyr);library(lubridate)

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
data <- readr::read_csv(file.path("raw_data", "property.csv.gz"))
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



#Crime data



ifiles <- unzip(file.path(".","raw_data", "census_blk.zip"), list = T)
ifiles.name <- substr(ifiles[,1][1], 1, nchar(ifiles[,1][1]) - 4)

if(!file.exists( file.path(".","wip", ifiles.name ) )){
  dir.create(file.path(".","wip", ifiles.name ))
  unzip(file.path(".","raw_data", "census_blk.zip"), 
        files = grep("*[.]", ifiles[,1], value = T), exdir = file.path(".","wip",ifiles.name), junkpaths = T)
} 

##Load data from crime data that contains street,Neighborhood and Police District info

data <- readr::read_csv(file.path("raw_data", "street_crime.csv.gz"))
names(data)[c(4,11)] <- c("Street",names(data)[4])
dat1 <- data #subset(data, select = -Location)
subset(data, select = c("Street", "Neighborhood", "Location", "CrimeDate")) -> data

data <- data[!is.na(data$Location),]

new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)

#side note. Records have been geocoded to the hundredth block and not the precise point that the crime took place.
data <- data.frame(data[,c(1,2, 4)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))
dat1$lon <- NA;dat1$lat <- NA
dat1[!is.na(dat1$Location),][,-11] <- data.frame(dat1[!is.na(dat1$Location),][,-c(11,13,14)],
                                               lon = as.numeric(new_loc[,2]), 
                                               lat = as.numeric(new_loc[,1]))
dat1 <- subset(dat1, select = -Location)

#Step 1
### Check if Lng and Lat fall inside polygons from ESRI Shape file for Child and wellbeing (this has the outcome)
dat.le <-  rgdal::readOGR(file.path("wip", "blk2010"), "blk2010")
block <- as.character(dat.le$BLOCK)
dat.le <- sp::spTransform(dat.le, sp::CRS("+proj=longlat +datum=WGS84")) #SpatialPolygonsDataFrame


# Assignment modified according
sp::coordinates(data) <- ~lon + lat #SpatialPointsDataFrame

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
sp::proj4string(data) <- sp::proj4string(dat.le)

sp::over(dat.le, data, returnList = T) -> block_crime #gives which Neighborhood belongs to what crime block


unlink(file.path("wip", "blk2010"), force = T, recursive = T)


names(block_crime) <- block

#Gives a dataframe with cols Block, Neighborhood and street 
block_crime <- plyr::ldply (block_crime, data.frame) 
clnames <- names(block_crime)
clnames[1] <- "Blocks"
names(block_crime) <- clnames
block_crime$Neighborhood <- toupper(block_crime$Neighborhood)

block_crime <- block_crime %>% 
  mutate(year = year(as.Date(CrimeDate, "%m/%d/%Y")))
block_crime <- subset(plyr::arrange(block_crime, Neighborhood, Blocks, year, Street, CrimeDate), 
                      select = c(Neighborhood, Blocks, year, Street, CrimeDate)) 

block_crime <- unique(block_crime)


#Step 2
##Load data from real properties that contains block info
data <- readr::read_csv(file.path("raw_data", "property.csv.gz"))
dat2 <- subset(data, select = names(data)[c(2:3,6,8,9,11,13:15)])
subset(data, select = c("Block", "Neighborhood", "Location")) -> data

data <- na.omit(data)

new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)
data <- data.frame(data[,c(1,2)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))

dat2$lon <- NA;dat2$lat <- NA
dat2[!is.na(dat2$Neighborhood),] <- data.frame(dat2[!is.na(dat2$Neighborhood),][,1:9],
                                               lon = as.numeric(new_loc[,2]), 
                                               lat = as.numeric(new_loc[,1]))

# Assignment modified according
sp::coordinates(data) <- ~lon + lat #SpatialPointsDataFrame

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
sp::proj4string(data) <- sp::proj4string(dat.le)

sp::over(dat.le, data, returnList = T) -> block_prop  #gives which Neighborhood-block belongs to what real property Neighbhd-block

names(block_prop) <- block

#Gives a dataframe with cols Block, Neighborhood and street 
block_prop <- plyr::ldply (block_prop, data.frame) 
clnames <- names(block_prop)
clnames[1] <- "Blocks"
names(block_prop) <- clnames
block_prop$Neighborhood <- toupper(block_prop$Neighborhood)

block_prop <- subset(plyr::arrange(block_prop, Neighborhood, Blocks, Block), select = c(Neighborhood, Blocks, Block)) 
block_prop <- unique(block_prop)


# Get which block numeration corresponds to street block in real_prop taxes

dplyr::inner_join(block_crime,block_prop) -> block_crime_pop

dat1$Neighborhood <- toupper(dat1$Neighborhood)

## Need to summarize otherwise the vanilla form is ~7.9 GB

block_crime_pop %>% 
  inner_join(dat1[,-c(12,13)]) %>% #Removing lon and lat since it is approximate for crime data
  inner_join(dat2[,c(1,4:7, 10:11)]) %>%
  mutate(CityTax = gsub("\\$", "", CityTax), CityTax = as.numeric(CityTax),
         StateTax = gsub("\\$", "", StateTax), StateTax = as.numeric(StateTax),
         AmountDue = gsub("\\$", "", AmountDue), AmountDue = as.numeric(AmountDue), 
         TotalIncidents = as.numeric(`Total Incidents`)) %>%
  group_by(Neighborhood, Block, year) %>% 
  summarise(TotalIncidents = sum(TotalIncidents, na.rm = T),
            mean(CityTax, na.rm = T),
            mean(StateTax, na.rm = T),
            mean(AmountDue, na.rm = T),
            median(lon, na.rm = T),
            median(lat, na.rm = T)) -> block_crime_pop

#Step 3
##Load data from real properties that contains block info
data <- readr::read_csv(file.path("raw_data", "vacants.csv"))
dat3 <- subset(data, select = names(data)[c(2,4:6)])
subset(data, select = c("BuildingAddress", "Neighborhood", "Location", "NoticeDate")) -> data

data <- na.omit(data)

new_loc <-  sapply(data$Location, function(x) {
  y <- substr(x, start = 2, stop = nchar(x) - 1)
  strsplit(y, ", ")[[1]]
}
)
new_loc <- t(new_loc)
data <- data.frame(data[,c(1,2, 4)], lon = as.numeric(new_loc[,2]), lat = as.numeric(new_loc[,1]))

dat3$lon <- NA;dat3$lat <- NA
data.frame(dat3[!is.na(dat3$BuildingAddress) & !is.na(dat3$Neighborhood) ,][,1:4],
                                               lon = as.numeric(new_loc[,2]), 
                                               lat = as.numeric(new_loc[,1])) -> dat3[!is.na(dat3$BuildingAddress) & !is.na(dat3$Neighborhood),]

#Convert Noticedate to date obj and then extract the year
dat3 %>% 
  mutate(year =  year(as.Date(NoticeDate, "%m/%d/%Y")),Neighborhood = toupper(Neighborhood)) -> dat3

# Assignment modified according
sp::coordinates(data) <- ~lon + lat #SpatialPointsDataFrame

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
sp::proj4string(data) <- sp::proj4string(dat.le)

sp::over(dat.le, data, returnList = T) -> block_vac  #gives which Neighborhood-block belongs to what vacant property Neighbhd-block


names(block_vac) <- block

#Gives a dataframe with cols Block, Neighborhood and street 
block_vac <- plyr::ldply (block_vac, data.frame) 
clnames <- names(block_vac)
clnames[1] <- "Blocks"
names(block_vac) <- clnames
block_vac$Neighborhood <- toupper(block_vac$Neighborhood)

block_vac <- subset(plyr::arrange(block_vac, Neighborhood, Blocks, BuildingAddress, NoticeDate), 
                    select = c(Neighborhood, Blocks, BuildingAddress, NoticeDate)) 
block_vac <- unique(block_vac)

# Get which block numeration corresponds to street block in real_prop taxes

#Join to real_property taxes dataset by Neighbourhood and block(numerical) to get which street and Neighbourhood correspond to what 
  #to what Neighbourhood and block in real_property taxes

dplyr::inner_join(block_vac,block_prop) -> block_vac_pop 

block_vac_pop %>% 
  inner_join(dat3, by  = c("Neighborhood", "BuildingAddress", "NoticeDate")) %>% #Join to vacant_building dataset by street, nbhd, and date
  group_by(Neighborhood, Block = Block.x, Year = year) %>% #Group by Neighbourhood, block and year to obtain summary stats
  summarise(Count_vancant = length(BuildingAddress)) -> block_vac_pop #Remove NA tags. This NA tags represents vacancy but for unknown years

rm(dat.le,dat1, dat2, dat3, block, 
   new_loc, ifiles, ifiles.name,clnames, data)


##For block_crime_pop and block_vac_pop, I want the "years' variable to rep all the possible years between 2010 and 2016
block_crime_pop -> block_crime_pop1
block_crime_pop1 %>% 
  select(Neighborhood, Block) %>%
  unique() -> block_crime_pop1

block_crime_pop1[rep(seq_len(nrow(block_crime_pop1)), each=5),] -> block_crime_pop1

block_crime_pop1$year <- rep(c(2010:2014), n_distinct(block_crime_pop1))

names(block_crime_pop) <- c(names(block_crime_pop)[1:4], "CityTax.avg",
                            "StateTax.avg", "AmountDue.avg",
                            "lon.med", "lat.med")

block_crime_pop1 %>% 
  left_join(block_crime_pop) -> crime_pop 
  # mutate(TotalIncidents = ifelse(is.na(TotalIncidents), 0, TotalIncidents),
  #        CityTax.avg = ifelse(is.na(CityTax.avg), 0, CityTax.avg),
  #        StateTax.avg = ifelse(is.na(StateTax.avg), 0, StateTax.avg),
  #        AmountDue.avg = ifelse(is.na(AmountDue.avg), 0, AmountDue.avg) ) -> crime_pop
crime_pop_impute <- crime_pop %>%
  subset(select = -Block) %>%
  group_by(Neighborhood, year) %>%
  summarise_all(mean, na.rm = T)
# Impute neighbourhood average for the particular year
attach(crime_pop)
crime_pop[is.na(TotalIncidents), ][,c(1,3)] %>%
  inner_join(crime_pop_impute) -> crime_pop[is.na(TotalIncidents), ][,-2]
crime_pop[is.na(CityTax.avg), ][,c(1,3)] %>%
  inner_join(crime_pop_impute) -> crime_pop[is.na(CityTax.avg), ][,-2]
crime_pop[is.na(StateTax.avg), ][,c(1,3)] %>%
  inner_join(crime_pop_impute) -> crime_pop[is.na(StateTax.avg), ][,-2]
crime_pop[is.na(AmountDue.avg), ][,c(1,3)] %>%
  inner_join(crime_pop_impute) -> crime_pop[is.na(AmountDue.avg), ][,-2]

#After imputation some years just have missing info
crime_pop %>%
  mutate(TotalIncidents = ifelse(is.na(TotalIncidents), 0, TotalIncidents),
         CityTax.avg = ifelse(is.na(CityTax.avg), 0, CityTax.avg),
         StateTax.avg = ifelse(is.na(StateTax.avg), 0, StateTax.avg),
         AmountDue.avg = ifelse(is.na(AmountDue.avg), 0, AmountDue.avg) ) -> crime_pop

detach(crime_pop)

block_vac_pop -> block_vac_pop1
block_vac_pop1 %>% 
  select(Neighborhood, Block) %>%
  unique() -> block_vac_pop1

block_vac_pop1[rep(seq_len(nrow(block_vac_pop1)), each=5),] -> block_vac_pop1

block_vac_pop1$Year <- rep(c(2010:2014), n_distinct(block_vac_pop1))

names(block_vac_pop) <- c(names(block_vac_pop)[1:3], "Count_vacant")

block_vac_pop1 %>% 
  left_join(block_vac_pop)  -> vac_pop
vac_pop$Count_vacant <- ifelse(is.na(vac_pop$Count_vacant), 0, vac_pop$Count_vacant)

rm(block_crime_pop1, block_vac_pop1,block_crime_pop, block_vac_pop)
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
detach("package:lubridate", unload=TRUE)

setwd(file.path(".."))