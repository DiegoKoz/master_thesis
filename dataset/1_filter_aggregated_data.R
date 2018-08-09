rm(list = ls())
gc()
library(tidyverse)


junto_tablas <- function(path = "Dataset/"){
  DF <- tibble()
  
  directorios <- paste0(path,list.files(path))
  
  for (dir in directorios) {
    data <- readRDS(dir)
    agg0 <-data %>% filter(`Aggregate Level`==0) 
    rm(data)
    DF <- bind_rows(DF,agg0)
    
  }
  return(DF)
}

agregado <- junto_tablas(path = "Dataset/")
outfile <- "aggregated_trade.RDS"
saveRDS(agregado,outfile)