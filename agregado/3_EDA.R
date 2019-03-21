rm(list = ls())
gc()
library(tidyverse)
library(igraph)
library(countrycode)
library(ggthemes)
library(countrycode)
library(ggrepel)
library(openxlsx)
library(glue)

countrycode_data <- codelist %>% select(cldr.short.es_ar, cldr.short.en,iso3c,region,continent)

dataset <- readRDS('agregado/dataset/dataset_COMTRADE_2016.rds')


dataset <- dataset %>% select(pt3ISO,rt3ISO,TradeValue, ptTitle)
dataset2 <- left_join(dataset,codelist %>% select(pt3ISO=iso3c, continent))

##Threshold of 1% del comercio de dicho país. Chow 2013

dataset2 <- dataset2 %>% 
  group_by(rt3ISO) %>% 
  mutate(L = case_when(TradeValue>sum(TradeValue)*0.01~1,
                       TRUE ~ 0)) %>% 
  na.omit(.)

edges <- dataset2 %>% filter(L==1)

g <- graph_from_data_frame(edges,directed = TRUE)

plot(g,edge.arrow.size = 0.1, size =10, curved=TRUE)
lay <- - layout.fruchterman.reingold(g)
plot(g, layout=lay, color = "continent")


### aristas ponderadas

E(g)$weight <- edges$TradeValue

plot(g, edge.width=E(g)$weight/max(E(g)$weight),
     alpha = E(g)$weight/max(E(g)$weight), vertex.size=10,
     edge.arrow.size = .1, edge.arrow.width = .1, 
     loop.angle = 2, asp = .75)

## threshold 2%
dataset2 <- dataset2 %>% 
  group_by(rt3ISO) %>% 
  mutate(L = case_when(TradeValue>sum(TradeValue)*0.02~1,
                       TRUE ~ 0)) %>% 
  na.omit(.)

edges <- dataset2 %>% filter(L==1)

g <- graph_from_data_frame(edges,directed = TRUE)

plot(g,edge.arrow.size = 0.1, size =10, curved=TRUE)


############# Relaciones dependientes

################## trade_to_plot #############
dataset <- readRDS('agregado/dataset/aggregated_trade.RDS') %>% 
  filter(ptCode != 0, TradeValue>100, rgCode == 1)
dataset2 <-  dataset %>% select(yr,rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue) %>% 
  left_join(countrycode_data %>% 
              select(rt3ISO=iso3c, continent, cldr.short.es_ar)) %>% 
  na.omit(.) %>% 
  group_by(yr,rt3ISO) %>% 
    mutate(pcnt = TradeValue/sum(TradeValue)) %>% 
    na.omit(.) %>% 
  filter(pcnt>.7)
  

dataset2 %>% 
  mutate(dupla = glue('{rtTitle}-{ptTitle}')) %>% 
  ggplot(., aes(yr,pcnt, label=dupla, size= TradeValue ))+
  geom_text_repel()+
  theme_tufte()+
  labs(x= 'Año', y= 'Porcentaje')+
  theme(legend.justification = 'left', 
        legend.position = 'bottom',
        legend.box = 'vertical', 
        legend.box.just = 'left',
        text = element_text(size=25))+
  guides(size=guide_legend("Monto"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(1996,2017,4))


ggsave("agregado/graficos/relaciones_dependientes_EDA.png", scale = 2)


##### interacciones- valor

dataset <- readRDS('agregado/dataset/aggregated_trade.RDS') %>% 
  filter(ptCode != 0, TradeValue>100, rgCode == 1)

dataset2 <-  dataset %>% select(yr,rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue) %>% 
  left_join(countrycode_data %>% 
              select(rt3ISO=iso3c, continent, cldr.short.es_ar)) %>% 
  na.omit(.) %>% 
  group_by(yr,rtTitle,ptTitle) %>% 
  summarise(Valor = sum(TradeValue)) %>% 
  na.omit(.) %>% 
  group_by(yr) %>% 
  top_n(.,5,Valor)

dataset2 %>% 
  mutate(dupla = glue('{rtTitle}-{ptTitle}')) %>% 
  ggplot(., aes(yr,Valor, label=dupla, size= Valor/1000000000 ))+
  geom_text_repel()+
  theme_tufte()+
  labs(x= 'Año', y= 'Valor Comercial. Miles de millones')+
  theme(legend.justification = 'left', 
        legend.position = 'bottom',
        legend.box = 'vertical', 
        legend.box.just = 'left',
        text = element_text(size=25))+
  guides(size=guide_legend("Tamaño"))+
  scale_y_continuous(labels = function(x)x/1000000000)+
  scale_x_continuous(breaks = seq(1996,2017,4))


ggsave("agregado/graficos/relaciones_destacadas_EDA.png", scale = 2)


#############
dataset %>% select(yr,rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue) %>% 
  left_join(countrycode_data %>% 
              select(rt3ISO=iso3c, continent, cldr.short.es_ar)) %>% 
  na.omit(.) %>% 
  group_by(yr,rtTitle,ptTitle) %>% 
  summarise(Valor = sum(TradeValue)) %>% 
  na.omit(.) %>% 
  ungroup() %>% 
  mutate(yr = yr+runif(n(),-.5,.5)) %>% 
  ggplot(.,aes(yr, Valor))+
  geom_hex()+
  theme_tufte()+
  labs(x= 'Año', y= 'Valor Comercial. Miles de millones')+
  theme(legend.justification = 'left', 
        legend.position = 'bottom',
        legend.box = 'vertical', 
        legend.box.just = 'left',
        text = element_text(size=25))+
  guides(fill=guide_legend("Tamaño"))+
  scale_y_continuous(labels = function(x)x/1000000000, 
                     breaks = seq(0, 500000000000, 100000000000))+
  scale_x_continuous(breaks = seq(1996,2017,4))

ggsave("agregado/graficos/relaciones_96_2016_EDA.png", scale = 2)

