
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


clasificacion <-read_xlsx("names/UN Comtrade Commodity Classifications.xlsx")
clasificacion_lall <-read_xlsx("names/Correspondencia SITCR3a3-d--Lall.xlsx")
codigos_paises <- read_csv("names/codigos_paises.csv")

etiquetas_componentes <- read_csv("names/Etiquetas tópicos LDA - ETIQUETAS.csv")

# resultados <- list.files("results/")
# for (resultado in resultados) {
#   assign(sub(".csv","",resultado),read_csv(glue("results/{resultado}")))
# }

iteraciones <- c("Dist_paises2.csv","Dist_paises4.csv","Dist_paises8.csv","Dist_paises10.csv",
  "Dist_paises20.csv","Dist_paises40.csv")


read_and_sum <- function(x) {
  read_csv(glue("results/{x}")) %>% 
    gather(componente,prop,3:ncol(.)) %>% 
    group_by(componente) %>% 
    summarise(prop=mean(prop)) %>% 
    mutate(k=str_extract(x,"[:digit:]{1,2}"))
}

df <- map(iteraciones,read_and_sum) %>% bind_rows()

df %>% 
  mutate(k =as.numeric(k),
         componente = as.numeric(componente)+1) %>% 
  ggplot(.,aes(componente,prop))+
  geom_col()+
  facet_grid(k~.,labeller = label_both)+
  labs(x="Componente",y="Proporción")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.margin=margin(t = -.5,unit = "cm"), 
        panel.border =element_rect(fill = NA), 
        text = element_text(size=25), 
        strip.text.y = element_text(angle = 0))
ggsave("graficos/componentes.png",scale = 2)  
