setwd(file.path(".."))  
source(file.path(".", "directories.R"))

dir_create()

match.package <- function(){
  
  #list of packages that will be used
  list.of.packages <- c("ggplot2", "Rcpp", "lubridate", "downloader", 
                        "readr", "readxl", "maptools", "RColorBrewer",
                        "ggmap", "rgeos", "broom", "rgdal", "grDevices",
                        "animation", "ade4", "sp", "ape", "geosphere", "dplyr",
                        "plyr", "pryr", "tidyr", "gstat", "spdep", "spgwr",
                        "GWmodel", "ModelMap", "acs", "tigris") 
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) {
    # install.packages(new.packages)
    stop(paste("Please install the following packages", paste0(new.packages,collapse = " ")))
    print(paste("The following packages are missing", new.packages))
    x <- readline("Would you like to install them now?[y/n] >")
    if (any(x %in% c("y", "n")) & x == "y")
    {
      install.packages(new.packages)
    }
    else if (!any(x %in% c("y", "n")))
      print("Please enter y or n")
    else
      stop(paste("Please install the following packages", paste0(new.packages,collapse = " ")))
  }
  
}

match.package()

