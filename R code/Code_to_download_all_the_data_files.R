setwd(file.path("Data"))  
packages <- c("ggplot2","lubridate", "downloader", 
              "readr", "readxl", "maptools", "RColorBrewer", "ggmap", "devtools")
sapply(packages, library, character.only = T, quietly = T)

Real_Property_Taxes <- "https://data.baltimorecity.gov/api/views/27w9-urtv/rows.csv?accessType=DOWNLOAD"
Parks <- "https://data.baltimorecity.gov/api/views/3r8a-uawz/rows.csv?accessType=DOWNLOAD"
Religious_Buildings <- "https://data.baltimorecity.gov/api/views/kbdc-bpw3/rows.csv?accessType=DOWNLOAD"
Libraries <- "https://data.baltimorecity.gov/api/views/tgtv-wr5u/rows.csv?accessType=DOWNLOAD"
Liquor_Licenses <- "https://data.baltimorecity.gov/api/views/xv8d-bwgi/rows.csv?accessType=DOWNLOAD"
Customer_Service_Requests_311 <- "https://data.baltimorecity.gov/api/views/9agw-sxsr/rows.csv?accessType=DOWNLOAD"
Assisted_Living_Facilities <- "https://data.baltimorecity.gov/api/views/q2vm-e9dp/rows.csv?accessType=DOWNLOAD"
Adult_Day_Care_Facilities <- "https://data.baltimorecity.gov/api/views/yc75-xbrv/rows.csv?accessType=DOWNLOAD"
Nursing_Homes <- "https://data.baltimorecity.gov/api/views/53js-3bkd/rows.csv?accessType=DOWNLOAD"
Census_Profile_by_Neighborhood_Statistical_Areas_2010 <- "https://data.baltimorecity.gov/api/views/5iam-bd6p/rows.csv?accessType=DOWNLOAD"
Census_Demographics_2010 <- "https://data.baltimorecity.gov/api/views/cix3-h4cy/rows.csv?accessType=DOWNLOAD"
Neighborhood_Action_Sense_of_Community_2010 <- "https://data.baltimorecity.gov/api/views/ipje-efsv/rows.csv?accessType=DOWNLOAD"
Real_Property <- "http://gisdata.baltimorecity.gov/datasets/b41551f53345445fa05b554cd77b3732_0.csv"
CSA_to_NSA_2010 <- "http://bniajfi.org/wp-content/uploads/2014/04/CSA-to-NSA-2010.xlsx"
Census_Blocks_and_NSAs_2010 <- "http://bniajfi.org/wp-content/uploads/2014/04/Census-Blocks-and-NSAs-2010.xlsx"
BPD_Part_1_Victim_Based_Crime_Data <- "https://data.baltimorecity.gov/api/views/wsfq-mvij/rows.csv?accessType=DOWNLOAD"

#####     All 2010 to 2014 data     #####

# Noticed that demo data for 2010-2014 has only partial info
Census_Demographics_2010_to_2014 <- "https://data.baltimorecity.gov/api/views/t7sb-aegk/rows.csv?accessType=DOWNLOAD"
Census_Demographics_2010_complete <- "https://data.baltimorecity.gov/api/views/cix3-h4cy/rows.csv?accessType=DOWNLOAD"
Census_Demographics_2010_and_2012 <- "https://data.baltimorecity.gov/api/views/yp84-wh4q/rows.csv?accessType=DOWNLOAD"
Census_Demographics_2010_and_2013 <- "https://data.baltimorecity.gov/api/views/7pnq-8ebe/rows.csv?accessType=DOWNLOAD"



Children_and_Family_Health_Well_Being_2010_to_2014 <- "https://data.baltimorecity.gov/api/views/rtbq-mnni/rows.csv?accessType=DOWNLOAD"
Housing_and_Community_Development_2010_to_2014 <- "https://data.baltimorecity.gov/api/views/mvvs-32jm/rows.csv?accessType=DOWNLOAD"
Crime_Safety_2010_to_2014 <- "https://data.baltimorecity.gov/api/views/qmw9-b8ep/rows.csv?accessType=DOWNLOAD"
Workforce_and_Economic_Development_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Workforce-2010-2014.xlsx"
Arts_and_Culture_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Arts-2011-2014.xlsx"
Education_and_Youth_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Education-2010-2014.xlsx"
Sustainability_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Sustainability-2010-2014.xlsx"

#####      CODEBOOK     #####
BNIA_Vital_Signs_Codebook <- "https://data.baltimorecity.gov/api/views/ryvy-9zw6/rows.csv?accessType=DOWNLOAD"
if(!file.exists(file.path("..", "Text","codebook.csv"))){
  download(url = BNIA_Vital_Signs_Codebook, destfile = file.path("..", "Text","codebook.csv"),mode="wb")
}



#####     Downloading the files und save ze dates    #####
####      1 Neighbourhood data      ####
# data_path <- file.path(".", "Data")
data_names <- c("census.csv", "child_and_fam_wellbeing.csv", "housing.csv",
                "crime.csv", "workforce.xlsx", "culture.xlsx", "edu_and_youth.xlsx", 
                "sustain.xlsx", "census10.csv", "census12.csv", "census13.csv")
data_urls <- c(Census_Demographics_2010_to_2014, Children_and_Family_Health_Well_Being_2010_to_2014,
               Housing_and_Community_Development_2010_to_2014, Crime_Safety_2010_to_2014,
               Workforce_and_Economic_Development_2010_to_2014, Arts_and_Culture_2010_to_2014,
               Education_and_Youth_2010_to_2014, Sustainability_2010_to_2014, Census_Demographics_2010_complete,
               Census_Demographics_2010_and_2012, Census_Demographics_2010_and_2013)


mapply(function(x,y) {
  if(!file.exists("raw_data")){
    dir.create("raw_data")
  }
  if(!file.exists(file.path(".", "raw_data",y))){
    download(url = x, destfile = file.path(".", "raw_data",y),mode="wb")
    date_downloaded <- now()
    write.table(date_downloaded, file.path(".", "raw_data", "date_downloaded.txt"))
  }
},data_urls,data_names)

#####     2 Data that could ID Blocks within a Neighbourhood     #####
data_names <- c("csa_nsa.xlsx", "blocks_nsa.xlsx","property.csv", "parks.csv", "religious.csv",
                "libraries.csv", "cust_311.csv", "real_property.csv","street_crime.csv") 

#cust_311 has zip, address and neighbourhood

data_urls <- c(CSA_to_NSA_2010,Census_Blocks_and_NSAs_2010,
               Real_Property_Taxes, Parks, Religious_Buildings, 
               Libraries, Customer_Service_Requests_311,Real_Property,
               BPD_Part_1_Victim_Based_Crime_Data)


mapply(function(x,y) {
  if(!file.exists("raw_data")){
    dir.create("raw_data")
  }
  v <- list.files(file.path(".", "raw_data"), pattern = "*.csv")
  lv <- length(v)
  if(!file.exists(file.path(".", "raw_data",y)) & lv < 14){
    download(url = x, destfile = file.path(".", "raw_data",y),mode="wb")
    date_downloaded <- now()
    write.table(date_downloaded, file.path(".", "raw_data", "date_downloaded.txt"))
    if(file.info(file.path(".", "raw_data",y))$size*1e-6 > 30 )
      system(paste("gzip", file.path(".", "raw_data",y)))
  }
},data_urls,data_names)



####      Shape_files     ####
zip_Census_Demographics_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Census.zip"
zip_Children_and_Family_Health_Well_Being_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Health.zip"
zip_Housing_and_Community_Development_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Housing.zip"
zip_Crime_Safety_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Crime.zip"

zip_Workforce_and_Economic_Development_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Workforce.zip"
zip_Arts_and_Culture_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Arts.zip"
zip_Education_and_Youth_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Education.zip"
zip_Sustainability_2010_to_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Sustainability.zip"
zip_census_block_2010 <- "http://www.mdp.state.md.us/msdc/census/cen2010/maps/tiger10/blk2010.zip" # 2010 shapefile showing block info
zip_neighbour <- "https://data.baltimorecity.gov/download/ysi8-7icr/application%2Fzip"

data_names <- c("census.zip", "child_and_fam_wellbeing.zip", "housing.zip",
                "crime.zip", "workforce.zip", "culture.zip", "edu_and_youth.zip", 
                "sustain.zip", "census_blk.zip", "neighbour.zip")
data_urls <- c(zip_Census_Demographics_2010_to_2014, zip_Children_and_Family_Health_Well_Being_2010_to_2014,
               zip_Housing_and_Community_Development_2010_to_2014, zip_Crime_Safety_2010_to_2014,
               zip_Workforce_and_Economic_Development_2010_to_2014, zip_Arts_and_Culture_2010_to_2014,
               zip_Education_and_Youth_2010_to_2014, zip_Sustainability_2010_to_2014, zip_census_block_2010,
               zip_neighbour)


mapply(function(x,y) {
  if(!file.exists("raw_data")){
    dir.create("raw_data")
  }
  if(!file.exists(file.path(".", "raw_data",y))){
    download(url = x, destfile = file.path(".", "raw_data",y),mode="wb")
    date_downloaded <- now()
    write.table(date_downloaded, file.path(".", "raw_data", "date_downloaded.txt"))
  }
},data_urls,data_names)

###   Extract the zip files   ###

##All except neighbour and block data
sapply(data_names[1:8], function(x){
  if(!file.exists("wip")){
    dir.create("wip")
  }
  no_files_expected <- 7*length(data_names)
  
  if (length(list.files(file.path(".","wip"))) < no_files_expected){
    unzip(file.path(".","raw_data", x), 
          # files = grep("*.shp|*.dbf|*.shx",unzip(file.path(".","raw_data", x), list = T)[,1], value = T),
          exdir = file.path(".","wip"), junkpaths = T)
  }
  
})

##rename them
sapply(list.files(file.path(".","wip"), # pattern="*.shp|*.dbf|*.shx"
), 
function(x){
  file.rename(from =file.path(".","wip", x), 
              to = file.path(".","wip", tolower(gsub("^.*?_", "", x, ignore.case = T)) ) )
}
)

##    Create a sub dir under WIP for each dataset and move the file into it
shape_dirs <- sapply(list.files(file.path(".","wip")), function(x){
  strsplit(x, "[.]")[[1]][1]
}
)


sapply(shape_dirs, function(x){
  if(!file.exists( file.path(".","wip",x)) ){
    dir.create(file.path(".","wip",x))
    files_to_copy <- list.files(file.path(".","wip"), pattern= paste0(x,"[.]*"))[-1]
    file.copy( paste0(file.path(".","wip", files_to_copy)) ,
               file.path(file.path(".","wip", x)) )
    file.remove(paste0(file.path(".","wip", files_to_copy)))
  }
  else if (file.exists( file.path(".","wip",x))){
    files_to_del <- list.files(file.path(".","wip"), pattern= paste0(x,"[.]*"))[-1]
    file.remove(paste0(file.path(".","wip", files_to_del)))
  }
}
)

#Block data
ifiles <- unzip(file.path(".","raw_data", "census_blk.zip"), list = T)
ifiles.name <- substr(ifiles[,1][1], 1, nchar(ifiles[,1][1]) - 4)
if(!file.exists( file.path(".","wip", ifiles.name ) ) & length(list.files(file.path("..","Plots"))) == 0){
  dir.create(file.path(".","wip", ifiles.name ))
  unzip(file.path(".","raw_data", "census_blk.zip"), 
        files = grep("*[.]", ifiles[,1], value = T), exdir = file.path(".","wip",ifiles.name), junkpaths = T)
}


#Neighbourhood data
ifiles <- unzip(file.path(".","raw_data", "neighbour.zip"), list = T)
ifiles.name <- substr(ifiles[,1][1], 1, 5)
if(!file.exists( file.path(".","wip", ifiles.name ) )){
  dir.create(file.path(".","wip", ifiles.name ))
  unzip(file.path(".","raw_data", "neighbour.zip"), 
        files = grep("*[.]", ifiles[,1], value = T), exdir = file.path(".","wip",ifiles.name), junkpaths = T)
  file.to.del <- list.files(file.path(".","wip", ifiles.name ), pattern = ".shp|.atx", full.names = T)[-2]
  # system(paste("rm", file.to.del[1], file.to.del[2]))
}





setwd(file.path(".."))
