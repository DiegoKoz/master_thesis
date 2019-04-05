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

resultados <- list.files("results/")

for (resultado in resultados) {
  assign(sub(".csv","",resultado),read_csv(glue("results/{resultado}")))
  
}

#### funciones y preprocesamiento ####

graficar <- function(df,paises, download= F) {

  g <- df%>%
    filter(reporter %in% paises) %>%
    group_by(rep_iso, Componente) %>% 
    mutate(prop = case_when(any(prop>0.05)~prop,
                                TRUE~NA_real_)) %>% 
    ungroup() %>% #tmp
    ggplot(., aes(year,prop, color = Componente))+
    geom_line(alpha=0.7, size=1)+
    geom_point(size=1)+
    theme_minimal()+
    theme(legend.position = "none")+
    facet_wrap(reporter~., ncol = 1, scales = "free")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    geom_dl(aes(label = Componente), method = list(dl.trans(x = x + .2), "last.points")) +
    geom_dl(aes(label = Componente), method = list(dl.trans(x = x - .2), "first.points"))
    
  if (download) {
    g <- g+
      theme(text = element_text(size=15),
            axis.text.x = element_text(size = 15))+
      scale_x_continuous(breaks = seq(1996,2017,4))+
      labs(x="Año", y= "Proporción")
      # labs(caption="Componentes con peso mayor al 5%")
  }
  if (!download) {
    g <- g + 
      scale_x_continuous(limits = c(min(unique(df$year)),max(unique(df$year))),
                         breaks = unique(df$year))+
      labs(caption="Components with weight greater than 5%")
  }
  
  g
  
}

graficar_lall <- function(k, comp, download = F){
  
  g <- dfs %>% 
    filter(K==k) %>%
    unnest() %>% 
    filter(componente == comp) %>%
    unnest() %>% 
    select(-K) %>% 
    group_by(LDC, LDC_description) %>% 
    summarise(prop=sum(prop)) %>% 
    ggplot(., aes(LDC,prop, fill = LDC_description))+
    geom_col()+
    scale_y_continuous(labels = percent,name = "Proporción")+
    theme_minimal()+
    theme(legend.position = "none")
  
  if (download) {
    g <- g+
      theme(text = element_text(size=20))
  }
  if (!download) {
    g <- g + 
      labs(caption="Components with weight greater than 5%")
  }
  g
}

preprocesamiento <- function(dfs) {
  nested_df <- tibble()
  for (df in dfs) {
    df_tmp <- df %>%
      mutate(componente = c(1:nrow(.))) %>%
      select(componente, everything()) %>%
      gather(Code, prop, 2:ncol(.)) %>%
      left_join(., clasificacion, by = "Code") %>%
      mutate(Code3=substr(Code,1,3)) %>% 
      left_join(., clasificacion_lall, by= "Code3") %>%
      arrange(componente, -prop) %>%
      group_by(componente) %>%
      mutate(cumprop = cumsum(prop)) %>%
      select(componente, Code, Description, prop, cumprop,LDC,LDC_description ) %>%
      nest() %>% 
      mutate(K=n())
    nested_df <- bind_rows(nested_df,df_tmp)
  }  
  nested_df %>% 
    group_by(K) %>% 
    nest()
}

clasificacion <- clasificacion %>%
  filter(Classification == "S3") %>% 
  select(Code, Description)%>%
  mutate(Code = case_when(Code %in% c("334", "673", "676") ~paste0(Code,"0"),
                          TRUE ~ Code))

# cadenas ####
dfs <- list(Dist_cadenas2,Dist_cadenas4,Dist_cadenas6,Dist_cadenas8,Dist_cadenas10,
            Dist_cadenas20,Dist_cadenas30,Dist_cadenas40,
            Dist_cadenas100,Dist_cadenas200) %>% 
  preprocesamiento(.)
  

dfs_paises <- c("Dist_paises2","Dist_paises4","Dist_paises6","Dist_paises8","Dist_paises10",
                   "Dist_paises20","Dist_paises30","Dist_paises40", 
                "Dist_paises100","Dist_paises200")


for (df_pais in dfs_paises) {
  assign(df_pais, get(df_pais) %>%
           left_join(codigos_paises,by = "rep_iso") %>%
           select(reporter, everything()) %>% 
           gather(Componente, prop, 4:ncol(.)) %>% 
           mutate(Componente= factor(as.numeric(Componente)+1)))
}




##### UI #####
 

#Functions for UI
supertab_dist <- function(K){
  tabPanel(glue("{K} Components"),
           h1(glue("{K} Components")),
           h3("Here you can see the weight of each product in each component"),
           do.call(tabsetPanel,
                   lapply(1:K, function(comp) {
                     tabPanel(glue("Component {comp}"),
                              dataTableOutput(glue("comp_{K}_{comp}")),
                              h3("distribution of the component according to the Lall's classification"),
                              downloadButton(glue('downloadcomp_{K}_{comp}_lall'), 'Download Plot'),
                              fluidRow(column(width = 6,
                     plotOutput(glue("comp_{K}_{comp}_lall"), width = "700px", height = "500px")
                              ),
                     column(width = 6,
                            dataTableOutput(glue("comp_{K}_{comp}_lall_desc")))
                     ))
                   }))
  )
}
supertab_paises <- function(K){
  tabPanel(glue("{K} Components"),
           sidebarPanel(
             selectInput(
               glue("paises_{K}"), "Reporting country:",
               c(unique(Dist_paises20$reporter)),
               selected = c("Argentina","Brasil","Uruguay","Paraguay"),
               multiple = TRUE),
             submitButton("Update countries", icon("refresh")),
             hr(),
             downloadButton(glue('downloadPlot_{K}'), 'Download Plot'),
             hr(),
             br(),
             h4("Component labels"),
             dataTableOutput(glue("labels_{K}")) 
           ),
           mainPanel(
             h3("Evolution of the participation of each component in each country."),
              # plotlyOutput(glue("plot{K}"), width = "800px", height = "600px")%>%
             plotOutput(glue("plot{K}"), width = "800px", height = "600px")%>%
               withSpinner(color="#0dc5c1")

             )
  )
  
}



ui <- fluidPage(
  theme = shinytheme("paper"),
  navbarPage(
    "Latent Dirichlet Allocation Models",
    tabPanel("Explanation(es)",
             fluidRow(column(10,br(),withMathJax(includeMarkdown("explicacion.Rmd"))))),

    navbarMenu(
      "Components",
    supertab_dist(K = 2),
    supertab_dist(K = 4),
    supertab_dist(K = 6),
    supertab_dist(K = 8),
    supertab_dist(K = 10),
    supertab_dist(K = 20),
    supertab_dist(K = 30),
    supertab_dist(K = 40),
    supertab_dist(K = 100),
    supertab_dist(K = 200)
    #dataTableOutput("Lall_desc")
    ),
    navbarMenu(
      "Countries",
      supertab_paises(K = 2),
      supertab_paises(K = 4),
      supertab_paises(K = 6),
      supertab_paises(K = 8),
      supertab_paises(K = 10),
      supertab_paises(K = 20),
      supertab_paises(K = 30),
      supertab_paises(K = 40),
      supertab_paises(K = 100),
      supertab_paises(K = 200)
    )
  )
)


##### server #####

server <- function (input, output) {

  # Componentes ####
  #Tabla
  
  lapply(c(2,4,6,8,10,20,30,40,100,200),function(k){
       lapply(1:k,function(comp){
      output[[glue("comp_{k}_{comp}")]] <- renderDataTable(
        datatable(  
          dfs %>% 
            filter(K==k) %>%
            unnest() %>% 
            filter(componente == comp) %>%
            unnest() %>% 
            select(-K,-LDC,-LDC_description),
          options = list(scrollX = TRUE)
        ) %>%
          formatStyle(
            c("prop", "cumprop"),
            background = styleColorBar(c(0, 1), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>%
          formatPercentage(c("prop", "cumprop"))
      )
      #Lall
      output[[glue("comp_{k}_{comp}_lall")]] <- renderPlot({graficar_lall(k, comp)})
      
      output[[glue("downloadcomp_{k}_{comp}_lall")]] <- downloadHandler(
        
        filename = function() { glue('graficoLall_k{k}_comp{comp}.png') },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
          ggsave(file, plot = graficar_lall(k, comp, download = T),scale=2, device = device)
        }
      )
      
      output[[glue("comp_{k}_{comp}_lall_desc")]] <- renderDataTable(
        datatable(
          clasificacion_lall %>% 
            select(-Code3) %>% unique(.),
          options = list(scrollX = TRUE)
        ))
       
      
      })
        output[[glue("labels_{k}")]] <- renderDataTable(
      datatable(
        etiquetas_componentes %>% 
          filter(K==k) %>%
          select(-K),
        options = list(scrollX = TRUE),
        rownames= FALSE
      ))
    
  })

  
 #Graficos #### 
  
  
  lapply(c(2,4,6,8,10,20,30,40,100,200),function(k){
    
    paises_graf <- reactive({input[[glue('paises_{k}')]]    })
    output[[glue("plot{k}")]] <- renderPlot({
    # output[[glue("plot{k}")]] <- renderPlotly({
      # ggplotly(graficar(get(glue('Dist_paises{k}')),paises_graf()))})
      graficar(get(glue('Dist_paises{k}')),paises_graf())})

  })
 
  
  
  lapply(c(2,4,6,8,10,20,30,40,100,200),function(k){
    
    paises_graf <- reactive({input[[glue('paises_{k}')]]    })
    
    output[[glue("downloadPlot_{k}")]] <- downloadHandler(
      
      filename = function() { glue('graficoLDA_k{k}.png') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, 
               plot = graficar(get(glue('Dist_paises{k}')),paises_graf(), download = T),
               scale=2,
               device = device)
      }
    )
  })
    
  
}

##### RUN #####

shinyApp(ui, server)

