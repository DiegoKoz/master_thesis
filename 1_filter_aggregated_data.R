rm(list = ls())
gc()
library(tidyverse)


junto_tablas <- function(path = "dataset/Dataset/"){
  DF <- tibble()
  
  directorios <- paste0(path,list.files(path))
  
  for (archivo in directorios) {
    data <- readRDS(archivo)
    agg0 <-data %>%  
      select(yr = Year, periodDesc = `Period Desc.`,aggrLevel  = `Aggregate Level`, rgCode = `Trade Flow Code`, 
             rgDesc = `Trade Flow`, rtCode = `Reporter Code`, rtTitle = Reporter, rt3ISO = `Reporter ISO`,
             ptCode = `Partner Code`, ptTitle = Partner, pt3ISO = `Partner ISO`, cmdCode = `Commodity Code`,
             cmdDescE = Commodity, TradeValue = `Trade Value (US$)`) %>% 
      filter(rgCode %in% c(1,2),
             aggrLevel == 0)
    
    rm(data)
    gc()
    DF <- bind_rows(DF,agg0)
    
  }
  return(DF)
}

agregado <- junto_tablas(path = "dataset/Dataset/")

#agregado <- read_rds("aggregated_trade.RDS")
# 
# agregado <- agregado %>% 
#   select(yr = Year, periodDesc = `Period Desc.`,aggrLevel  = `Aggregate Level`, rgCode = `Trade Flow Code`, 
#          rgDesc = `Trade Flow`, rtCode = `Reporter Code`, rtTitle = Reporter, rt3ISO = `Reporter ISO`,
#          ptCode = `Partner Code`, ptTitle = Partner, pt3ISO = `Partner ISO`, cmdCode = `Commodity Code`,
#          cmdDescE = Commodity, TradeValue = `Trade Value (US$)`) %>% 
#   filter(rgCode %in% c(1,2))
# 

outfile <- "dataset/aggregated_trade.RDS"
saveRDS(agregado,outfile)
