rm(list = ls())
gc()
library(tidyverse)
library(ggridges)

dataset <- readRDS('dataset/aggregated_trade.RDS')


  
  


resumen <- dataset %>%
  group_by(yr) %>% 
  summarise(n = n(),
            suma = sum(TradeValue, na.rm = TRUE),
            promedio = mean(TradeValue, na.rm = TRUE)) %>% 
  gather(.,variable, valor, 2:4)



ggplot(resumen, aes(yr, valor))+
  geom_col()+
  theme_minimal()+
  #scale_x_continuous(breaks = seq(1997,2016,1))+
  facet_grid(variable~., scales =  'free')

tabla <-as.data.frame(table(dataset$yr,dataset$rtTitle))

tab <- tabla %>% 
  filter(Freq>0) %>% 
  group_by(Var1) %>% 
  summarise(n())


#dataset2 <- dataset %>% filter(yr<= 2011)


dataset_2016 <- dataset %>% filter(yr == 2016,ptCode != 0, TradeValue>100, rgCode == 1)

ggplot(dataset_2016, aes(TradeValue))+
  geom_density(fill = "lightgreen")+
  theme_minimal()

saveRDS(dataset_2016, "dataset/dataset_COMTRADE_2016.rds")



dataset_2016_expo <- dataset %>% filter(yr == 2016,ptCode != 0, TradeValue>100, rgCode == 2)

ggplot(dataset_2016_expo, aes(TradeValue))+
  geom_density(fill = "lightgreen")+
  theme_minimal()

saveRDS(dataset_2016_expo, "dataset/dataset_COMTRADE_expo_2016.rds")





