library(tidyverse)
library(ggthemes)
library(ggfortify)
library(treemapify)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(readxl)
library(DT)
##### datos #####


dataset <- read_rds("data_cadena.rds")
#renombro las variables
dataset <- dataset %>% 
  rename(reporter = Reporter, partner = Partner, year = Year, flow = TradeFlowName, value = TradeValue)

dataset <-   dataset %>%
  mutate(partner=case_when(partner=="RDM_Sudamerica_sin_China"~"Rest of the world \n (except China)",
                           partner=="Bolivia (Plurinational State of)" ~ "Bolivia",
                           TRUE ~ partner))


agregados <- c("Mercosur", "Sudamerica","RDM_Mercosur","RDM_Mercosur_sin_China","RDM_Sudamerica", "World")

dataset <- dataset %>% 
  filter(!partner %in% agregados)

tabla_cadenas <- read_xlsx("leyendas_treemaps.xlsx",sheet = "CADENAS")
tablas_subcadenas <- read_xlsx("leyendas_treemaps.xlsx",sheet = "SUBCADENAS")
tablas_usos <- read_xlsx("leyendas_treemaps.xlsx",sheet = "USOS")

##### funciones y presets de datos##### 

grow_treemap_partner <- function(data=dataset, nano= 2015, reportante = "Argentina",sin_rdm = T, sin_china=T){
  
  paises <- unique(data$partner)
  #colores = rainbow(length(paises), v=0.6,alpha = 0.9)
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
  names(colores) <- paises
  
  if (reportante == "Sudamerica") {
    data_filt <-   data %>% 
      filter(year == nano)
    
  }else{
    data_filt <-   data %>% 
      filter(reporter==reportante, year == nano)
  }
  if (sin_rdm == TRUE){
    data_filt <-   data_filt %>% 
      filter(partner != "Rest of the world \n (except China)")
  } 
  if (sin_china == TRUE){
    data_filt <-   data_filt %>% 
      filter(partner != "China")
  }
  if (nrow(data_filt)==0) {
    return(ggplot()+
      ggtitle(paste("No data for ",reportante,", ",nano)))
    
  }else{
    
    data_filt  %>% 
      group_by(partner,flow) %>% 
      summarise(value = sum(value)) %>%
      group_by(flow) %>% 
      mutate(prop= scales::percent(value/sum(value)),
             label = glue("{partner}\n{prop}")) %>% 
      ggplot(., aes(area = value, label = partner, fill= partner)) + 
      geom_treemap(layout = "fixed")+
      geom_treemap_text(aes(label=prop),colour = "white", place = "topright",
                        grow = F, layout = "fixed")+
      geom_treemap_text(colour = "black", place = "left",
                        grow = F, layout = "fixed")+
      facet_grid(.~flow)+
      labs(title= glue("Treemap {reportante}, year {nano}"))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = 'none',
            strip.text = element_text(family="Times", face="bold", size=20))

  }
}

grow_treemap_cadsubcad <- function(data = dataset,nano=2015, reportante = "Argentina", flow_filter = "Export"){
  
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
      filter(year == nano,flow== flow_filter)
    
  }else{
    data_filt <-   data %>% 
      filter(reporter==reportante, year == nano, flow == flow_filter)
  }
  if (nrow(data_filt)==0) {
    return(ggplot()+
             ggtitle(paste("No hay data para ",reportante,", ",nano)))    
  }else{
    
    data_filt  %>% 
      mutate(RDM = case_when(partner=="Rest of the world \n (except China)"~partner,
                             partner=="China"~partner,
                             TRUE ~ "South America"),
             RDM = factor(RDM, levels = c("South America","Rest of the world \n (except China)","China"))) %>%  
      group_by(Cadena,Subcadena,RDM,flow) %>% 
      summarise(value = sum(value)) %>% 
      ggplot(., aes(area = value, fill = Cadena, label = Subcadena, subgroup = Cadena)) + 
      geom_treemap( layout = "fixed", alpha = 1)+
      geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0,  layout = "fixed") +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T,  layout = "fixed")+
      facet_wrap(~RDM, ncol = 2)+
      labs(title= glue("Treemap {flow_filter}, {reportante}, year {nano}"))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = "None",
            title = element_text(size = 18),
            strip.text = element_text(size=18))

    
  }
}

grow_treemap_caduse <- function(data=dataset,nano= 2015, reportante = "Argentina",flow_filter = "Export"){
  
  cadenas <- unique(data$Cadena)
  #Tomo toda la escala cromática, y eligo por k-means los colores más distintivos posibles
  colores <- c("#455930",  "#c057c5",
               "#63b649","#6148b9","#c49d35","#7a8fd0","#d1503b","#5ea7a3",
               "#c7407f","#80a25e","#56426d","#bc8960","#c787a5","#793933")
  names(colores) <- cadenas
  
  
  if (reportante == "Sudamerica") {
    data_filt <-   data %>% 
      filter(year == nano,flow== flow_filter)
    
  }else{
    data_filt <-   data %>% 
      filter(reporter==reportante, year == nano, flow == flow_filter)
  }
  if (nrow(data_filt)==0) {
    return(ggplot()+
             ggtitle(paste("No hay data para ",reportante,", ",nano)))
    
  }else{
    
    data_filt  %>% 
      mutate(RDM = case_when(partner=="Rest of the world \n (except China)"~partner,
                             partner=="China"~partner,
                             TRUE ~ "South America"),
             RDM = factor(RDM, levels = c("Rest of the world \n (except China)","China"))) %>%  
      group_by(RDM,Cadena,Flor) %>% 
      summarise(value = sum(value)) %>% 
      ggplot(., aes(area = value, fill = Cadena, label = Flor, subgroup = Cadena)) + 
      geom_treemap( layout = "fixed", alpha = 1)+
      geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0,  layout = "fixed") +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T,  layout = "fixed")+
      facet_wrap(~RDM, ncol = 2)+
      labs(title= glue("Treemap {flow_filter}, {reportante}, year {nano}"))+
      scale_fill_manual(values = colores)+
      theme_tufte()+
      theme(legend.position = "None",
            title = element_text(size = 18),
            strip.text = element_text(size=18))
  }
}

##### UI ##### 

ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("Treemaps",
                           tabPanel("Countries",
                                    sidebarPanel(selectInput("reportante", "Reporting country:",
                                                             c("All South America"="Sudamerica",unique(dataset$reporter))),
                                                 checkboxInput("sin_rdm", "Exclude rest of the world (except China)", value = TRUE),
                                                 checkboxInput("sin_china", "Exclude China", value = TRUE),
                                                 sliderInput("nano","year:",
                                                             min =min(dataset$year), max = max(dataset$year), value = 1996, 
                                                             animate= animationOptions(interval = 1500))
                                                 ),
                                    mainPanel( plotOutput("paises", width = "800px", height = "600px"),
                                               downloadButton(glue('downloadPlot_paises'), 'Download Plot'),
                                               helpText("Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")),
                                               helpText("the code can be found in the",
                                                        a("GitHub repo", href="https://github.com/DiegoKoz/southamerica_commerce_graph"))
                                    )
                           ),
                           tabPanel("Chains",
                                    
                                      sidebarPanel(
                                        selectInput("reportante_cad", "Reporting country:",
                                                    c("All South America"="Sudamerica",unique(dataset$reporter))),
                                        selectInput("flow", "Type of flow:", unique(dataset$flow)),
                                        sliderInput("nano_cad","Year:",min =min(dataset$year),
                                                    max = max(dataset$year), value = 1996,
                                                    animate= animationOptions(interval = 1500))
                                        ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Chains and subchains",
                                                             plotOutput("cadsubcad", width = "800px", height = "600px")%>% 
                                                               withSpinner(color="#0dc5c1"),
                                                             downloadButton(glue('downloadPlot_cadsubcad'), 'Download Plot'),
                                                             h5("Subchains description"),
                                                             dataTableOutput("subcadenas", width =600)
                                                             ),
                                                    tabPanel("Chains and uses",
                                                             plotOutput("caduse", width = "800px", height = "600px")%>%
                                                               withSpinner(color="#0dc5c1"),
                                                             downloadButton(glue('downloadPlot_chains'), 'Download Plot'),
                                                               fixedRow(
                                                                 column(width = 5,
                                                                        h5("Chains description"),
                                                                        dataTableOutput("cadenas",width = 350)
                                                                        ),
                                                                 column(width = 5,
                                                                        h5("Uses description"),
                                                                        dataTableOutput("usos", width =400)
                                                                        )
                                                               )
                                                             ),
                                        helpText("Author: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")),
                                        helpText("The definition of the chains and subchains is available in the following",
                                                 a("Working document",href="http://bibliotecadigital.econ.uba.ar/download/docin/docin_iiep_010")),
                                        helpText("The code can be found in the following",
                                                 a("GitHub repo", href="https://github.com/DiegoKoz/southamerica_commerce_graph"))
                                        )
                                        )
                                    )
                           )
                )



##### server ##### 

server <- function (input, output) {
  
  output$paises =  renderPlot({
    grow_treemap_partner(data = dataset,reportante = input$reportante, nano = input$nano, sin_rdm= input$sin_rdm, sin_china = input$sin_china)
  })
  output$cadsubcad =  renderPlot({
    grow_treemap_cadsubcad(data = dataset,reportante = input$reportante_cad, nano = input$nano_cad, flow_filter = input$flow)
  })
  output$caduse =  renderPlot({
    grow_treemap_caduse(data = dataset, reportante = input$reportante_cad, nano = input$nano_cad, flow_filter = input$flow)
  })
  
  output$cadenas <- renderDataTable(
    tabla_cadenas,
    options = list(scrollX = TRUE)
  )
  
  output$subcadenas <- renderDataTable(
    tablas_subcadenas,
    options = list(scrollX = TRUE)
  )
  
  output$usos <- renderDataTable(
    tablas_usos,
    options = list(scrollX = TRUE)
  )
  
  output[["downloadPlot_paises"]] <- downloadHandler(
    
    filename = function() { glue('treemaps_cadsubcad.png') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      
      ggsave(file,
             plot = grow_treemap_partner(data = dataset,reportante = input$reportante, nano = input$nano, sin_rdm= input$sin_rdm, sin_china = input$sin_china),
             device = device)
    }
  )
  output[["downloadPlot_cadsubcad"]] <- downloadHandler(
    
    filename = function() { glue('treemaps_cadsubcad.png') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file,
             plot =    grow_treemap_cadsubcad(data = dataset,reportante = input$reportante_cad, nano = input$nano_cad, flow_filter = input$flow),
             device = device)
    }
  )
  output[["downloadPlot_chains"]] <- downloadHandler(
    
    filename = function() { glue('treemaps_cadsubcad.png') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file,
             plot = grow_treemap_caduse(data = dataset, reportante = input$reportante_cad, nano = input$nano_cad, flow_filter = input$flow),
             device = device)
    }
  )
  
}

##### RUN ##### 

shinyApp(ui, server)





