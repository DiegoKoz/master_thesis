rm(list = ls())
gc()
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggfortify)
#library(plotly)
library(knitr)
library(factoextra)
library(cluster)
library(viridis)
library(treemapify)
library(jsonlite)
library(scales)
library(LaplacesDemon)
library(RColorBrewer)
library(gganimate)


data <- read_rds("data/data_cadena.rds")
#renombro las variables
data <- data %>% 
  rename(reporter = Reporter, partner = Partner, year = Year, flow = TradeFlowName, value = Valor.en.1000.USD)

agregados <- c("Mercosur", "Sudamerica","RDM_Mercosur", "World")
data_filt <- data %>% 
  filter(!partner %in% agregados)



##### Videos treemaps por cadena y subcadena y uso, expo e impo #######
grow_treemap_cadsubcad_gif <- function(data = data_filt, reportante = "Argentina"){
  
  cadenas <- unique(data$Cadena)
  #Tomo toda la escala cromática, y eligo por k-means los colores más distintivos posibles
  colores <- c("#455930",
               "#c057c5",
               "#63b649",
               "#6148b9",
               "#c49d35",
               "#7a8fd0",
               "#d1503b",
               "#5ea7a3",
               "#c7407f",
               "#80a25e",
               "#56426d",
               "#bc8960",
               "#c787a5",
               "#793933")
  names(colores) <- cadenas
  
  
  data_filt <-   data %>% 
    filter(reporter==reportante) 
  
  if (reportante == "Sudamerica") {
    data_filt <-   data
  }
  if (nrow(data_filt)==0) {
    print(paste("No hay data para ",reportante,", ",nano))
    
  }else{
    
    graf <- data_filt  %>% 
      group_by(Cadena,Subcadena, flow, year) %>% 
      summarise(value = sum(value)) %>%
      ggplot(., aes(area = value, fill = Cadena, label = Subcadena, subgroup = Cadena, frame = year)) + 
      geom_treemap( fixed =  TRUE, alpha = 1)+
      #geom_treemap_subgroup_border(fixed = TRUE)+
      geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0,  fixed =  TRUE) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T,  fixed =  TRUE)+
      # geom_treemap_text(colour = "white", place = "left",
      #                     grow = F)+
      facet_grid(.~flow)+
      labs(title= paste0('Treemap ', reportante, ".Año "))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = "None",
            title = element_text(size = 18),
            strip.text = element_text(size=18))
    
    
    
    gganimate(graf,filename =  paste0("graficos/treemap_byproduct_",reportante,".mp4"), 
              ani.width = 1600, ani.height = 900, interval = 0.3)  
    
  }
}
grow_treemap_caduse_gif <- function(data = data_filt, reportante = "Argentina"){
  
  cadenas <- unique(data$Cadena)
  #Tomo toda la escala cromática, y eligo por k-means los colores más distintivos posibles
  colores <- c("#455930",
               "#c057c5",
               "#63b649",
               "#6148b9",
               "#c49d35",
               "#7a8fd0",
               "#d1503b",
               "#5ea7a3",
               "#c7407f",
               "#80a25e",
               "#56426d",
               "#bc8960",
               "#c787a5",
               "#793933")
  names(colores) <- cadenas
  
  
  data_filt <-   data %>% 
    filter(reporter==reportante) 
  
  if (reportante == "Sudamerica") {
    data_filt <-   data
  }
  if (nrow(data_filt)==0) {
    print(paste("No hay data para ",reportante,", ",nano))
    
  }else{
    
    graf <- data_filt  %>% 
      group_by(Cadena,Flores,Cadena,flow, year) %>% 
      summarise(value = sum(value)) %>%
      ggplot(., aes(area = value, fill = Cadena, label = Flores, subgroup = Cadena, frame = year)) + 
      geom_treemap( fixed =  TRUE, alpha = 1)+
      #geom_treemap_subgroup_border(fixed = TRUE)+
      geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0,  fixed =  TRUE) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T,  fixed =  TRUE)+
      # geom_treemap_text(colour = "white", place = "left",
      #                     grow = F)+
      facet_grid(.~flow)+
      labs(title= paste0('Treemap ', reportante, ".Año "))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = "None",
            title = element_text(size = 18),
            strip.text = element_text(size=18))
    
    
    gganimate(graf,filename =  paste0("graficos/treemap_byproduc_tUse_",reportante,".mp4"), 
              ani.width = 1600, ani.height = 900, interval = 0.3)  
    
  }
}
######treemap con y sin RDM ########
grow_treemap_cadsubcad_rdm_gif <- function(data = data_filt, reportante = "Argentina", flow_filter = "Export"){
  
  cadenas <- unique(data$Cadena)
  #Tomo toda la escala cromática, y eligo por k-means los colores más distintivos posibles
  colores <- c("#455930",
               "#c057c5",
               "#63b649",
               "#6148b9",
               "#c49d35",
               "#7a8fd0",
               "#d1503b",
               "#5ea7a3",
               "#c7407f",
               "#80a25e",
               "#56426d",
               "#bc8960",
               "#c787a5",
               "#793933")
  names(colores) <- cadenas
  
  
  data_filt <-   data %>% 
    filter(reporter==reportante, flow== flow_filter) 
  
  if (reportante == "Sudamerica") {
    data_filt <-   data %>% 
      filter(flow== flow_filter) 
  }
  if (nrow(data_filt)==0) {
    print(paste("No hay data para ",reportante,", ",nano))
    
  }else{
    
    graf <- data_filt  %>% 
      mutate(RDM = case_when(partner=="RDM_Sudamerica"~"Resto del mundo",
                             TRUE ~ "Sudamérica")) %>% 
      group_by(Cadena,Subcadena, year,RDM) %>% 
      summarise(value = sum(value)) %>%
      ggplot(., aes(area = value, fill = Cadena, label = Subcadena, subgroup = Cadena, frame = year)) + 
      geom_treemap( fixed =  TRUE, alpha = 1)+
      #geom_treemap_subgroup_border(fixed = TRUE)+
      geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0,  fixed =  TRUE) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T,  fixed =  TRUE)+
      # geom_treemap_text(colour = "white", place = "left",
      #                     grow = F)+
      facet_grid(.~RDM)+
      labs(title= paste0('Treemap ',flow_filter,", ", reportante, ".Año "))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = "None",
            title = element_text(size = 18),
            strip.text = element_text(size=18))
    
    
    gganimate(graf,filename =  paste0("graficos/treemap_byproduct_RDM_",reportante,"_", flow_filter,".mp4"), 
              ani.width = 1600, ani.height = 900, interval = 0.3)  
    
  }
}

for (pais in c(paste(unique(data_filt$reporter)),"Sudamerica")) {
  grow_treemap_cadsubcad_gif(data = data_filt, reportante = pais)
}

for (pais in c(paste(unique(data_filt$reporter)),"Sudamerica")) {
  grow_treemap_caduse_gif(data = data_filt, reportante = pais)
}

for (pais in c(paste(unique(data_filt$reporter)),"Sudamerica")) {
  for (flow_filt in c("Export", "Import")) {
  grow_treemap_cadsubcad_rdm_gif(data = data_filt, reportante = pais,flow_filter = flow_filt)
  }
}

# for (pais in c(paste(unique(data_filt$reporter)),"Sudamerica")) {
#   for (flow_filt in c("Import")) {
#     grow_treemap_cadsubcad_rdm_gif(data = data_filt, reportante = pais,flow_filter = flow_filt)
#   }
# }

