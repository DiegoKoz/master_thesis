library(tidyverse)
library(readxl)

path = "Cadenas/data/CADENAS (RDM_sin_China)/"
archivos <- paste0(path,list.files(path))
data <- data_frame()
for (archivo in archivos) {
  tmp <- read_excel(archivo)
  data <- bind_rows(data,tmp)
}
rm(tmp)


#Limpieza
data <- data %>% 
  filter(Reporter!=Partner, TradeValue>=0) %>% 
  mutate(Subcadena = case_when(Subcadena == "LçCTEOS" ~ "LACTEOS",
                               TRUE ~ Subcadena))


print(object.size(data), units="Mb")

saveRDS(data,"Cadenas/data/data_cadena.rds")



#test
rm(list = ls())
data <- read_rds("Cadenas/data/data_cadena.rds")
print(object.size(data), units="Mb")
glimpse(data)
