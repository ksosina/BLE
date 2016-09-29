dir_create <- function(){
  dir_names <- c("R code", "Data", "Text", "Plots")
  sapply(dir_names, function(x) {
    if(!file.exists(x)){
      dir.create(x)
    }
  })
}


dir_create()