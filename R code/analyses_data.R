#Create all data files to be used during analysis
setwd(file.path("..","Data"))


# stop(print(substr(getwd(), 37, nchar(getwd()))))

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
census10.13 %>% dplyr::arrange_(.dots=names(census10.13)[1:20]) %>%
  dplyr::mutate(hhs10 = as.numeric(gsub("\\*|\\* |,", "", hhs10)),
         mhhi13 = as.numeric(gsub("\\*|\\* |,", "", mhhi13))) -> census10.13

#I noticed that mhhi12 has $ in front of each figure in census10.12 so I will replace em
census10.12  %>% dplyr::arrange_(.dots=names(census10.12)[1:20]) %>%
  dplyr::mutate(mhhi12 = gsub("\\$", "", mhhi12), mhhi12 = as.numeric(mhhi12)) -> census10.12



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
  dplyr::select(-CSA2010.y,-hhs10.y, CSA2010 = CSA2010.x)
rm(census10.12, census10.13, census10.14, c1,c2,c3,c4)


#### ID which neighborhoods fall into a CSA

##Load data from real properties that contains block info

data <- readr::read_csv(file.path("raw_data", "property.csv.gz"))

#get count of houses per block
data %>% dplyr::group_by(Neighborhood,Block) %>%
  dplyr::summarise(count_house = n()) -> houses

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


#Step 1
### Check if Lng and Lat fall inside polygons from ESRI Shape file for Child and wellbeing (this has the outcome)
dat.le <-  rgdal::readOGR(file.path("wip", "health"), "health", verbose = F)
csa <- as.character(dat.le$CSA2010)
dat.le <- sp::spTransform(dat.le, sp::CRS("+proj=longlat +datum=WGS84")) #SpatialPolygonsDataFrame



# Assignment modified according
sp::coordinates(data) <- ~lon + lat #SpatialPointsDataFrame
real_prop <- data

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
  dplyr::mutate(CityTax = gsub("\\$", "", CityTax), CityTax = as.numeric(CityTax),
         StateTax = gsub("\\$", "", StateTax), StateTax = as.numeric(StateTax),
         AmountDue = gsub("\\$", "", AmountDue), AmountDue = as.numeric(AmountDue)) %>%
  dplyr::group_by(CSA, Neighborhood, Block) %>%
  dplyr::summarise(CityTax.med = median(CityTax, na.rm = T), StateTax.med = median(StateTax, na.rm = T), 
            AmountDue.med = median(AmountDue, na.rm = T), lon.med = median(lon, na.rm = T), lat.med = median(lat)) -> csa.prop

# pryr::object_size(csa.prop)


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



ifiles <- unzip(file.path("raw_data", "census_blk.zip"), list = T)
ifiles.name <- substr(ifiles[,1][1], 1, nchar(ifiles[,1][1]) - 4)

if(!file.exists( file.path("wip", ifiles.name ) )){
  dir.create(file.path("wip", ifiles.name ))
  unzip(file.path("raw_data", "census_blk.zip"), 
        files = grep("*[.]", ifiles[,1], value = T), exdir = file.path("wip",ifiles.name), junkpaths = T)
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
dat.le <-  rgdal::readOGR(file.path("wip", "blk2010"), "blk2010", verbose = F)
block <- as.character(dat.le$BLOCK)
dat.le <- sp::spTransform(dat.le, sp::CRS("+proj=longlat +datum=WGS84")) #SpatialPolygonsDataFrame

#

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
  dplyr::mutate(year = year(as.Date(CrimeDate, "%m/%d/%Y")))
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
  dplyr::mutate(CityTax = gsub("\\$", "", CityTax), CityTax = as.numeric(CityTax),
         StateTax = gsub("\\$", "", StateTax), StateTax = as.numeric(StateTax),
         AmountDue = gsub("\\$", "", AmountDue), AmountDue = as.numeric(AmountDue), 
         TotalIncidents = as.numeric(`Total Incidents`)) %>%
  dplyr::group_by(Neighborhood, Block, year) %>% 
  dplyr::summarise(TotalIncidents = sum(TotalIncidents, na.rm = T),
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
  dplyr::mutate(year =  year(as.Date(NoticeDate, "%m/%d/%Y")),Neighborhood = toupper(Neighborhood)) -> dat3

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
  dplyr::inner_join(dat3, by  = c("Neighborhood", "BuildingAddress", "NoticeDate")) %>% #Join to vacant_building dataset by street, nbhd, and date
  dplyr::group_by(Neighborhood, Block = Block.x, Year = year) %>% #Group by Neighbourhood, block and year to obtain summary stats
  dplyr::summarise(Count_vancant = length(BuildingAddress)) -> block_vac_pop #Remove NA tags. This NA tags represents vacancy but for unknown years

rm(dat.le,dat1, dat2, dat3, block, 
   new_loc, ifiles, ifiles.name,clnames, data)


##For block_crime_pop and block_vac_pop, I want the "years' variable to rep all the possible years between 2010 and 2016
block_crime_pop -> block_crime_pop1
block_crime_pop1 %>% 
  dplyr::select(Neighborhood, Block) %>%
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
  dplyr::group_by(Neighborhood, year) %>%
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
  dplyr::mutate(TotalIncidents = ifelse(is.na(TotalIncidents), 0, TotalIncidents),
         CityTax.avg = ifelse(is.na(CityTax.avg), 0, CityTax.avg),
         StateTax.avg = ifelse(is.na(StateTax.avg), 0, StateTax.avg),
         AmountDue.avg = ifelse(is.na(AmountDue.avg), 0, AmountDue.avg) ) -> crime_pop

detach(crime_pop)

block_vac_pop -> block_vac_pop1
block_vac_pop1 %>% 
  dplyr::select(Neighborhood, Block) %>%
  unique() -> block_vac_pop1

block_vac_pop1[rep(seq_len(nrow(block_vac_pop1)), each=5),] -> block_vac_pop1

block_vac_pop1$Year <- rep(c(2010:2014), n_distinct(block_vac_pop1))

names(block_vac_pop) <- c(names(block_vac_pop)[1:3], "Count_vacant")

block_vac_pop1 %>% 
  left_join(block_vac_pop)  -> vac_pop
vac_pop$Count_vacant <- ifelse(is.na(vac_pop$Count_vacant), 0, vac_pop$Count_vacant)

rm(block_crime_pop1, block_vac_pop1,block_crime_pop, block_vac_pop)
rm(block_crime, block_prop, block_vac, crime_pop_impute, csa.prop)




##Create analyses data
names(csa.prop.health) <- tolower(names(csa.prop.health))
names(vac_pop) <- tolower(names(vac_pop))
names(crime_pop) <- tolower(names(crime_pop)) #Note that lat and lon here refer to that gotten from real_prop data
names(houses) <- tolower(names(houses))

#Co-ordinates
csa.prop.health %>% 
  dplyr::group_by(csa) %>%
  dplyr::summarise(lon.med.avg = mean(lon.med), lat.med.avg = mean(lat.med)) -> coord.lon_lat

## 2014


# crime_pop[crime_pop$year == 2014,]

csa.prop.health[,c(1:3, 12)] %>% 
  left_join(subset(crime_pop, year == 2014, select = -c(lon.med, lat.med))) %>%
  left_join(subset(vac_pop, year == 2014)) %>%
  dplyr::mutate(count_vacant = as.numeric(ifelse(is.na(count_vacant), 0, count_vacant)))-> health_prop_le_crime_vac_block

health_prop_le_crime_vac_block %>%
  inner_join(houses) %>%
  dplyr::mutate(prop.vacant = count_vacant/count_house) -> health_prop_le_crime_vac_block

#For any variables that are NA use the neighbourhood average of the csa they belong to for imputation
health_prop_le_crime_vac_block %>%
  dplyr::select(-year, -block, -lifeexp14) %>%
  dplyr::group_by(csa, neighborhood) %>%
  dplyr::summarise( totalincidents = mean(totalincidents, na.rm = T),
             citytax.avg = mean(citytax.avg, na.rm = T),
             statetax.avg = mean(statetax.avg, na.rm = T),
             amountdue.avg = mean(amountdue.avg, na.rm = T),
             prop.vacant = mean(prop.vacant, na.rm = T)) -> impute

health_prop_le_crime_vac_block[is.na(health_prop_le_crime_vac_block$totalincidents), c(1:4)] %>%
  inner_join(impute) -> health_prop_le_crime_vac_block[is.na(health_prop_le_crime_vac_block$totalincidents),][,-c(5, 10:11)]
rm(impute)

#For any variables that are still NA use the CSA average that they belong to for imputation
health_prop_le_crime_vac_block %>%
  dplyr::select(-year, -block, -lifeexp14) %>%
  dplyr::group_by(csa) %>%
  dplyr::summarise( totalincidents = mean(totalincidents, na.rm = T),
             citytax.avg = mean(citytax.avg, na.rm = T),
             statetax.avg = mean(statetax.avg, na.rm = T),
             amountdue.avg = mean(amountdue.avg, na.rm = T),
             prop.vacant = mean(prop.vacant, na.rm = T)) -> impute

health_prop_le_crime_vac_block[is.na(health_prop_le_crime_vac_block$totalincidents), c(1:4)] %>%
  inner_join(impute) -> health_prop_le_crime_vac_block[is.na(health_prop_le_crime_vac_block$totalincidents),][,-c(5, 10:11)]
rm(impute)


#Aggregate by CSA 

health_prop_le_crime_vac_block %>%
  dplyr::select(-year, -block, -neighborhood) %>%
  filter(!is.nan(totalincidents)) %>%
  # mutate(count_vacant = as.numeric(count_vacant)) %>%
  dplyr::group_by(csa) %>%
  summarise_all(mean) -> csa.data

census12_14 %>% dplyr::select(csa = CSA2010, tpop = tpop10, racdiv10,mhhi13, femhhs10) %>% 
  inner_join(csa.data) %>%
  subset(select = (c(csa, lifeexp14, tpop, 
                     racdiv10, mhhi13, femhhs10, 
                     totalincidents, citytax.avg, 
                     statetax.avg, amountdue.avg, 
                     prop.vacant ))) -> csa.data

housing <- readr::read_csv(file.path("raw_data", "housing.csv"))
edu <- readxl::read_excel(file.path("raw_data", "edu_and_youth.xlsx"))
welfare <- readr::read_csv(file.path("raw_data", "child_and_fam_wellbeing.csv"))
sustain <- readxl::read_excel(file.path("raw_data", "sustain.xlsx"))
crime <- readr::read_csv(file.path("raw_data", "crime.csv"))


names(housing)[1] <- "csa";names(edu)[1] <- "csa";names(welfare)[1] <- "csa";names(sustain)[1] <- "csa"
names(crime)[1] <- "csa"


edu[-1,] -> edu
edu <- tbl_df(data.frame(edu[,1],apply(edu[,-1],2,as.numeric)))
sustain[-1,] -> sustain
sustain <- tbl_df(data.frame(sustain[,1],apply(sustain[,-1],2,as.numeric)))


csa.data %>%
  inner_join(subset(housing, select = c(csa, shomes14,cashsa14, fore14))) %>%
  inner_join(subset(crime, select = c(csa, shoot11, gunhom13, narc12))) %>%
  inner_join(subset(edu, select = c(csa, abse14,absmd14, abshs14, susp13 ))) %>%
  inner_join(subset(welfare, select = c(csa, birthwt14, liquor14))) %>%
  inner_join(subset(sustain, select = c(csa, heatgas14, elheat14,wlksc11))) -> csa.data


csa.data.anal <- data.frame(csa.data[,1:2], csa.data[,3:26], coord.lon_lat[,c(2:3)])
rm(csa.data)


#Block level

csa.data.anal[,c(1:2,4,6 ,17,19,21,23:26)] %>% 
  inner_join(health_prop_le_crime_vac_block[,c(1:4,6,12)]) %>%
  inner_join(csa.prop.health[,c(1:3, 7:8)]) %>% unique() -> block_data.anal


block_data.anal <- data.frame(block_data.anal[,c(1,12:13)], block_data.anal[,c(3:11, 14:15)], block_data.anal[,16:17])



##American Community Survey 

library(acs)

#Block Group data

##Related CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE (get prop fem headed with kid <18)

cdbk <- acs.lookup(2014, 5, table.number="B11004")
hh <- acs.fetch(2014, span  = 5, 
                geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                table.number = "B11004", case.sensitive = F)
hh <- data.frame(hh@geography[,4:5],hh@estimate, row.names = 1:dim(hh@estimate)[1])

hh %>%
  dplyr::mutate(propfemhh = B11004_015/B11004_001) %>% 
  dplyr::select(tract, blockgroup, propfemhh) -> hh

#OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE (get prop fem headed with kid <18)
child.code <- acs.lookup(2014, 5, table.number="B09002")
child.ft <- acs.fetch(2014, span  = 5, 
                      geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                      table.number = "B09002", case.sensitive = F)
child.ft <- data.frame(child.ft@geography[,4:5],child.ft@estimate, row.names = 1:dim(child.ft@estimate)[1])



#POVERTY STATUS IN THE PAST 12 MONTHS BY DISABILITY STATUS BY EMPLOYMENT STATUS FOR THE POPULATION 20 TO 64 YEARS (get prop below pov line)
poverty.code <- acs.lookup(2014, 5, table.number="B23024")
pov.ft <- acs.fetch(2014, span  = 5, 
                    geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                    table.number = "B23024", case.sensitive = F)
pov.ft <- data.frame(pov.ft@geography[,4:5],pov.ft@estimate, row.names = 1:dim(pov.ft@estimate)[1])

pov.ft %>%
  dplyr::mutate(propbelow = B23024_002/B23024_001) %>% 
  dplyr::select(tract, blockgroup, propbelow) -> pov.ft


#MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2014 INFLATION-ADJUSTED DOLLARS)
medinc.code <- acs.lookup(2014, 5, table.number="B19013")
medinc.ft <- acs.fetch(2014, span  = 5, 
                       geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                       table.number = "B19013", case.sensitive = F)
medinc.ft <- data.frame(medinc.ft@geography[,4:5],medinc.ft@estimate, row.names = 1:dim(medinc.ft@estimate)[1])


#TYPES OF HEALTH INSURANCE COVERAGE BY AGE
insur.code <- acs.lookup(2014, 5, table.number="B27010")
insur.ft <- acs.fetch(2014, span  = 5, 
                      geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                      table.number = "B27010", case.sensitive = F)
insur.ft <- data.frame(insur.ft@geography[,4:5],insur.ft@estimate, row.names = 1:dim(insur.ft@estimate)[1])

insur.ft %>%
  dplyr::mutate(propkids_withinsurance = 1- (B27010_017/B27010_002)) %>% 
  dplyr::select(tract, blockgroup, propkids_withinsurance) %>% 
  dplyr::mutate(propkids_withinsurance = ifelse(is.nan(propkids_withinsurance),1, propkids_withinsurance)) -> insur.ft

#RACE "to calculate racial diversity"

# Working with percents expressed as ratios (e.g., 63 percent = 0.63), the index is calculated in three steps:  A. Square the percent for each
# group, B. Sum the squares, and C. Subtract the sum from 1.00. Eight groups were used for the index: 1. White, not Hispanic;
# 2. Black or African American; 3. American Indian and Alaska Native (AIAN); 4. Asian; 5. Native Hawaiian and Other Pacific
# Islander (NHOPI); 6. Two or more races, not Hispanic; 7. Some other race, not Hispanic; and 8. Hispanic or Latino

race.code <- acs.lookup(2014, 5, table.number="B02001")
race.ft <- acs.fetch(2014, span  = 5, 
                     geography=geo.make(state="MD", county = "Baltimore city", tract ="*", block.group ="*"), 
                     table.number = "B02001", case.sensitive = F)
race.ft <- data.frame(race.ft@geography[,4:5],race.ft@estimate, row.names = 1:dim(race.ft@estimate)[1])

race.ft %>%
  dplyr::mutate(B02001_002 = B02001_002/B02001_001, B02001_003 = B02001_003/B02001_001, B02001_004 = B02001_004/B02001_001,
         B02001_005 = B02001_005/B02001_001, B02001_006 = B02001_006/B02001_001, B02001_007 = B02001_007/B02001_001,
         B02001_010 = B02001_010/B02001_001, 
         racdiv = 1-(B02001_002^2 + B02001_003^2 + B02001_004^2 + B02001_005^2 + B02001_006^2 + B02001_007^2 + B02001_010^2)) %>% 
  dplyr::select(tract, blockgroup, racdiv) -> race.ft

library(tigris)
bmore.city <- block_groups("MD", "Baltimore city")

# BG <- as.character(dat.le$CSA2010)
bmore.city_sp <- sp::spTransform(bmore.city, sp::CRS("+proj=longlat +datum=WGS84")) #SpatialPolygonsDataFrame

bmore.city <- bmore.city_sp


rm(bmore.city_sp)
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
sp::proj4string(real_prop) <- sp::proj4string(bmore.city)

sp::over(bmore.city, real_prop, returnList = T) -> BG_neighbhd_csa 

BG_neighbhd_csa <- plyr::ldply(BG_neighbhd_csa, data.frame)

as.data.frame(real_prop) %>%
  dplyr::group_by(Neighborhood, Block) %>%
  dplyr::summarise(lon = median(lon), lat = median(lat)) -> areal.prop

BG_neighbhd_csa %>%
  dplyr::inner_join(dplyr::mutate(bmore.city@data, .id = row.names(bmore.city@data))) %>%
  dplyr::select(.id, Neighborhood, Block, tract = TRACTCE, blockgroup = BLKGRPCE) %>%
  dplyr::mutate(tract = as.numeric(tract)) %>%
  dplyr::left_join(na.omit(hh)) %>% 
  dplyr::left_join(na.omit(pov.ft)) %>%
  dplyr::left_join(na.omit(medinc.ft)) %>%
  dplyr::left_join(na.omit(insur.ft)) %>% 
  dplyr::left_join(na.omit(race.ft)) %>%
  dplyr::inner_join(areal.prop) %>%
  unique() %>% na.omit() -> bg_smooth

bg_smooth -> BG_neighbhd_csa

bg_smooth %>%
  dplyr::group_by(tract, blockgroup) %>%
  dplyr::summarise(propfemhh = mean(propfemhh), propbelow = mean(propbelow), 
            B19013_001 = mean(B19013_001), propkids_withinsurance = mean(propkids_withinsurance), 
            racdiv = mean(racdiv),lon = median(lon), lat = median(lat)) %>% data.frame -> bg_smooth
rm(areal.prop)



# Kriging the Block group predictors

#proportion below Poverty
sp::coordinates(bg_smooth) <- ~lon + lat
semivariog <- gstat::variogram(propbelow~1, locations=bg_smooth, data=bg_smooth)
# plot(semivariog)
#the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
model.variog<-gstat::vgm(psill=0.012, model="Sph", nugget=0.018, range=0.074)
fit.variog<-gstat::fit.variogram(semivariog, model.variog)
# plot(semivariog, fit.variog)

## now expand your range to a grid with spacing that you'd like to use in your interpolation
#here we will use 200m grid cells:
# grd <- expand.grid(x=seq(from=range(BG_neighbhd_csa$lon)[1], to=range(BG_neighbhd_csa$lon)[2], by=1e-04),
#                    y=seq(from=range(BG_neighbhd_csa$lat)[1], to=range(BG_neighbhd_csa$lat)[2], by=1e-04))

grd <- data.frame(x=BG_neighbhd_csa$lon,
                  y=BG_neighbhd_csa$lat)

## convert grid to SpatialPixel class
coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE

krig<-gstat::krige(formula=propbelow~1, locations=bg_smooth, newdata=grd, model=model.variog, nmax = 5)

krig.output <- as.data.frame(krig)
names(krig.output)[1:3]<-c("lon","lat","propbelow.pred")

BG_neighbhd_csa %>%
  dplyr::left_join(krig.output[,-4]) %>%
  dplyr::mutate(propbelow.pred = ifelse(is.na(propbelow.pred),propbelow, propbelow.pred))-> BG_neighbhd_csa

#Median Income
semivariog <- gstat::variogram(B19013_001~1, locations=bg_smooth, data=bg_smooth)
# plot(semivariog)
#the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
model.variog<-gstat::vgm(psill=751835064, model="Exp", nugget=257444639, range=0.074)
fit.variog<-gstat::fit.variogram(semivariog, model.variog)
# plot(semivariog, fit.variog)

krig<-gstat::krige(formula=B19013_001~1, locations=bg_smooth, newdata=grd, model=model.variog, nmax = 5)

krig.output <- as.data.frame(krig)
names(krig.output)[1:3]<-c("lon","lat","B19013_001.pred")

BG_neighbhd_csa %>%
  dplyr::left_join(krig.output[,-4]) %>%
  dplyr::mutate(mhhi = ifelse(is.na(B19013_001.pred),B19013_001, B19013_001.pred)) -> BG_neighbhd_csa


rm(krig.output, krig, fit.variog, model.variog, semivariog, grd, bg_smooth)

detach("package:dplyr", unload=TRUE)
detach("package:lubridate", unload=TRUE)
detach("package:acs", unload=TRUE)

save.image("analyses_data.RData")
setwd(file.path(".."))