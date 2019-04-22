
## LDA análisis
library(tidyverse)
library(ggthemes)
library(directlabels)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(markdown)
library(shinycssloaders)
library(glue)
library(readxl)
library(DT)
library(scales)
library(lubridate)
library(plotly)
#### datos #####
### Load ###


clasificacion <-read_xlsx("desagregado/LDA/names/UN Comtrade Commodity Classifications.xlsx")
clasificacion_lall <-read_xlsx("desagregado/LDA/names/Correspondencia SITCR3a3-d--Lall.xlsx")
codigos_paises <- read_csv("desagregado/LDA/names/codigos_paises.csv")

etiquetas_componentes <- read_csv("desagregado/LDA/names/Etiquetas tópicos LDA - ETIQUETAS.csv")


read_and_sum <- function(x) {
  read_csv(glue("desagregado/LDA/results/{x}")) %>% 
    gather(componente,prop,3:ncol(.)) %>% 
    group_by(componente) %>% 
    summarise(prop=mean(prop)) %>%
    ungroup() %>% 
    mutate(k=max(as.numeric(paste(componente))+1))
}
graficar <- function(lista_dist, file="desagregado/LDA/graficos/componentes.png") {
  

df <- map(lista_dist,read_and_sum) %>% bind_rows()

df %>% 
  mutate(k =as.numeric(k),
         componente = as.numeric(componente)+1) %>% 
  ggplot(.,aes(componente,prop))+
  geom_col()+
  facet_grid(k~.,labeller = label_both, scales = "free")+
  labs(x="Componente",y="Proporción")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.margin=margin(t = -.5,unit = "cm"), 
        panel.border =element_rect(fill = NA), 
        text = element_text(size=25), 
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(angle = 0))

ggsave(file,scale = 2)  
}


lista_dist <- c("Dist_paises_k2_etaDefault.csv",
                "Dist_paises_k4_etaDefault.csv",
                "Dist_paises_k6_etaDefault.csv",
                "Dist_paises_k8_etaDefault.csv",
                "Dist_paises_k10_etaDefault.csv",
                "Dist_paises_k20_etaDefault.csv",
                "Dist_paises_k30_etaDefault.csv",
                "Dist_paises_k40_etaDefault.csv",
                'Dist_paises_k50_etaDefault.csv')

graficar(lista_dist)

lista_dist <- c("Dist_paises2.csv","Dist_paises4.csv","Dist_paises8.csv","Dist_paises10.csv",
                "Dist_paises20.csv","Dist_paises40.csv",'Dist_paises50.csv','Dist_paises100.csv',
                'Dist_paises200.csv')

graficar(lista_dist,file="desagregado/LDA/graficos/componentes_all.png")



