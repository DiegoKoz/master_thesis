rm(list = ls())
gc()
library(tidyverse)


junto_tablas <- function(path = "dataset/Dataset/"){
  DF <- tibble()
  
  directorios <- paste0(path,list.files(path))
  ## Hay que sacar los aregados supranacionales
  
  pb <- txtProgressBar(min = 0, max = length(directorios), style = 3)
  stepi =1
  for (archivo in directorios) {
    setTxtProgressBar(pb, stepi)
    stepi = stepi+1
    data <- readRDS(archivo)
    eliminar <- c("World","Special Categories",unique(data$Partner)[grepl(" nes",unique(data$Partner))])
    # agg0 <-data %>%
    #   select(yr = Year, periodDesc = `Period Desc.`,aggrLevel  = `Aggregate Level`, rgCode = `Trade Flow Code`,
    #          rgDesc = `Trade Flow`, rtCode = `Reporter Code`, rtTitle = Reporter, rt3ISO = `Reporter ISO`,
    #          ptCode = `Partner Code`, ptTitle = Partner, pt3ISO = `Partner ISO`, cmdCode = `Commodity Code`,
    #          cmdDescE = Commodity, TradeValue = `Trade Value (US$)`) %>%
    #   filter(rgCode %in% c(1,2),
    #          cmdCode %in% c("TOTAL","total"),
    #          !ptTitle %in% eliminar)
    
    agg0 <- data %>% 
      select(yr = Year, periodDesc = `Period Desc.`,aggrLevel  = `Aggregate Level`, rgCode = `Trade Flow Code`, 
             rgDesc = `Trade Flow`, rtCode = `Reporter Code`, rtTitle = Reporter, rt3ISO = `Reporter ISO`,
             ptCode = `Partner Code`, ptTitle = Partner, pt3ISO = `Partner ISO`, cmdCode = `Commodity Code`,
             cmdDescE = Commodity, TradeValue = `Trade Value (US$)`) %>% 
      filter(rgCode %in% c(1,2), # me quedo con expo e impo
             !ptTitle %in% eliminar, # elimino los agregados supranacionales
             aggrLevel == 6) %>% #me quedo con el HS a 6 digitos nchar(cmdCode)==6
      group_by(yr, periodDesc,rgCode,rgDesc,rtCode,rtTitle, rt3ISO,ptCode,ptTitle, pt3ISO) %>% 
      summarise(TradeValue=sum(as.numeric(TradeValue),na.rm = T))
    
    rm(data)
    gc()
    DF <- bind_rows(DF,agg0)
    
  }
  
  return(DF)
}

agregado <- junto_tablas(path = "dataset/Dataset/")


# 
# ids <- agg_alt %>% ungroup() %>% select(rtTitle,ptTitle,rgDesc,TradeValueAlt=TradeValue) %>%
#   left_join(.,agg0 %>% ungroup() %>% select(rtTitle,ptTitle,rgDesc,TradeValue0=TradeValue))
# 
# ids$diff <- ids$TradeValueAlt-ids$TradeValue0
# plot(ids$diff)

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
