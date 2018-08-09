rm(list = ls())
library(tidyverse)
library(rjson)
library(qdapRegex)

get.Comtrade <- function(type="C",
                         freq="A",
                         r = "ALL",
                         px="HS",
                         ps,
                         token)
{
  string<- paste("http://comtrade.un.org/api/get/bulk/"
                 # ,"max=",maxrec,"&" #maximum no. of records returned
                 ,type,"/" #type of trade (c=commodities)
                 ,freq,"/" #frequency
                 ,ps,"/" #time period
                 ,r,"/" #reporting area
                 ,px
                 ,"?","token=",token
                 ,sep = ""
  )
      
  temp <- tempfile()
  download.file(string,temp)
  
  outdir = paste0(temp,"_tmp/")
  dir.create(outdir)
  unzip(temp,exdir = outdir)
  file_path <- paste0(outdir,list.files(outdir))
  data <- read_csv(file_path)
  unlink(temp)
  unlink(file_path)
  
  return(data)
  
}

periodos <- 1990:2017

for (periodo in periodos) {
  tryCatch({ 
  data <- get.Comtrade(ps = periodo,
                        token = "your token")
  outfile <- paste0("Dataset/",periodo,".RDS")
  saveRDS(data,outfile)
  rm(data)
  gc()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


