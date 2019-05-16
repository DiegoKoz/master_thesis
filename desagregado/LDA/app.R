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
library(ggrepel)
library(magrittr)
# library(countrycode)
# library(plotly)
#### datos #####
### Load ###


k_vec <- c(2,4,6,8,10,20,30,40,50)

#clasificacion <-read_xlsx("names/Comtrade_Commodity_Classifications.xlsx")
clasificacion <-read_csv("names/sitc_codes.csv")


clasificacion_lall <-read_xlsx("names/Corr_sitc_lall.xlsx")
codigos_paises <- read_csv("names/country_codes.csv")

etiquetas_componentes <- read_xlsx("names/LDA_ETIQUETAS.xlsx",sheet = 'LDA_temp')
# etiquetas_componentes <- read_csv("names/LDA_ETIQUETAS.csv")

resultados <- list.files("results/")
resultados <- resultados[str_detect(resultados,'etaDefault')]



for (resultado in resultados) {
  
  df_name <- str_replace_all(resultado,"_etaDefault.csv",replacement = '')
  df_name <- str_replace(df_name,'_k','')
  assign(df_name,read_csv(glue("results/{resultado}")))
  
}

loess_adj_max <- function(data){loess(prop ~ year, span = 0.8,data = data) %>% predict(newdata = max(data$year))}
loess_adj_min <- function(data){loess(prop ~ year, span = 0.8,data = data) %>% predict(newdata = min(data$year))}
#### funciones y preprocesamiento ####

graficar <- function(df,paises, download= F, smooth=F) {
  
  print(smooth)
  
  gdata <- df%>%
    filter(reporter %in% paises) %>%
    group_by(rep_iso, Componente) %>% 
    mutate(prop = case_when(any(prop>0.05)~prop,
                            TRUE~NA_real_)) %>% 
    ungroup() 
  g <-   ggplot(gdata, aes(year,prop, color = Componente,label = Componente))+
    theme_minimal()+
    theme(legend.position = "none")+
    facet_wrap(reporter~., ncol = 1, scales = "free")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1))+
    scale_x_continuous(breaks = pretty_breaks(n = 6))+
    labs(y='proportion')
  
  if (smooth) {
    g <- g + geom_smooth(se=F, alpha=0.7, size=1,method = 'loess')
    
    labelInfo <- gdata %>%
      filter(!is.na(prop)) %>% 
      group_by(reporter,Componente) %>%
      nest() %>% 
      mutate(predAtMax = unlist(map(data,loess_adj_max)),
             predAtMin = unlist(map(data,loess_adj_min)),
             max = unlist(map(data,function(df){max(df$year)})),
             min = unlist(map(data,function(df){min(df$year)}))) %>% 
                       select(reporter,Componente,predAtMax,predAtMin,max,min)


    
    g <- g + geom_text_repel(data = labelInfo, 
                             aes(x = max, y = predAtMax, label = Componente, color = Componente))+ 
      geom_text_repel(data = labelInfo,
                      aes(x = min, y = predAtMin, label = Componente, color = Componente),
                      nudge_y = 0.05) 
      
  }
  if (!smooth) {
    g <- g+  geom_line(alpha=0.7, size=1)+
      geom_point(size=1)+
      geom_text_repel(data = subset(gdata, year == max(year)),nudge_y = 0.05)+
      geom_text_repel(data = subset(gdata, year == min(year)),nudge_y = 0.05)
        
        # geom_dl(aes(label = Componente), method="smart.grid", inherit.aes=T)

  }
  if (download) {
    g <- g+
      theme(text = element_text(size=15),
            axis.text.x = element_text(size = 15))+
      #scale_x_continuous(breaks = seq(1996,2017,4))+
      labs(x="Año", y= "Proporción")
    # labs(caption="Componentes con peso mayor al 5%")
  }
  if (!download) {
    g <- g + 
      # scale_x_continuous(limits = c(min(unique(df$year)),max(unique(df$year))),
      #                    breaks = unique(df$year))+
      labs(caption="Components with weight greater than 5%")
  }
  
  g
  
}

graficar_dado_comp <- function(df, comp,n_paises=2,download=F, smooth=F ){
  
  n_paises <- switch (n_paises,
                      '1' = 1,
                      '2' = 2,
                      '3' = 3,
                      '4' = 4
                    ) #lo podria hacer con n <- integer(n)

  paises <- df %>% 
    filter(Componente==comp) %>% 
    group_by(reporter) %>% 
    summarise(prop = mean(prop)) %>% 
    arrange(-prop) %>% 
    top_n(n_paises,prop) %$%
    reporter
  
  graficar(df, paises,download = download,smooth = smooth)
  
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


clasificacion=clasificacion %>% select(Code=sitc_product_code, Description = sitc_product_name_short_en)


# cadenas ####
dfs <- list(Dist_cadenas2,Dist_cadenas4,Dist_cadenas6,Dist_cadenas8,Dist_cadenas10,
            Dist_cadenas20,Dist_cadenas30,Dist_cadenas40,Dist_cadenas50) %>% 
#             Dist_cadenas100,Dist_cadenas200) %>% 
   preprocesamiento(.)

envlist <- ls()

dfs_paises <- envlist[str_detect(envlist,'paises\\d+')]

codigos_paises <- codigos_paises %>% select(rep_iso= location_code, reporter = location_name_short_en)

for (df_pais in dfs_paises) {
  assign(df_pais, get(df_pais) %>%
           left_join(codigos_paises, by = 'rep_iso') %>% 
           # mutate(reporter= countrycode(rep_iso, 'iso3c', 'un.name.es', nomatch=NULL)) %>%
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
               selected = c("Argentina","Brasil"),
               multiple = TRUE),
             checkboxInput(glue("smooth_{K}"),"smooth",FALSE),
             hr(),
             submitButton("Update", icon("refresh")),
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

supertab_paises_dado_comp <- function(K){
  tabPanel(glue("{K} Components"),
           sidebarPanel(
             selectInput(
               glue("paises_comp_{K}"), "Component:",
               c(unique(get(glue('Dist_paises{K}'))[['Componente']])),
               selected = "1",
               multiple = FALSE),
             selectInput(
               inputId = glue("npaises_comp_{K}"),
               label = "Number of countries:",
               choices = c(1:4),
               selected=2,
               multiple=FALSE),
             checkboxInput(glue("smooth_comp_{K}"),"smooth",FALSE),
             hr(),
             submitButton("Update", icon("refresh")),
             hr(),
             downloadButton(glue('downloadPlot_comp_{K}'), 'Download Plot'),
             hr(),
             br(),
             h4("Component labels"),
             dataTableOutput(glue("labels_comp_{K}")) 
           ),
           mainPanel(
             h3("Top Countries by component"),
             # plotlyOutput(glue("plot{K}"), width = "800px", height = "600px")%>%
             plotOutput(glue("plot_comp{K}"), width = "800px", height = "600px")%>%
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
      supertab_dist(K = 50)
      # supertab_dist(K = 100),
      # supertab_dist(K = 200)
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
      supertab_paises(K = 50)
      # supertab_paises(K = 100),
      # supertab_paises(K = 200)
    ),
    navbarMenu(
      "Countries by comp",
      supertab_paises_dado_comp(K=2),
      supertab_paises_dado_comp(K=4),
      supertab_paises_dado_comp(K=6),
      supertab_paises_dado_comp(K=8),
      supertab_paises_dado_comp(K=10),
      supertab_paises_dado_comp(K=20),
      supertab_paises_dado_comp(K=30),
      supertab_paises_dado_comp(K=40),
      supertab_paises_dado_comp(K=50)      
    )
  )
)


##### server #####

server <- function (input, output) {
  
  # Componentes ####
  #Tabla
  
  lapply(k_vec,function(k){
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
    
    output[[glue("labels_comp_{k}")]] <- renderDataTable(
      datatable(
        etiquetas_componentes %>% 
          filter(K==k) %>%
          select(-K),
        options = list(scrollX = TRUE),
        rownames= FALSE
      ))
    
  })
  
  
  #Graficos #### 
  
  
  lapply(k_vec,function(k){
    
    paises_graf <- reactive({input[[glue('paises_{k}')]]    })
    output[[glue("plot{k}")]] <- renderPlot({
      # output[[glue("plot{k}")]] <- renderPlotly({
      # ggplotly(graficar(get(glue('Dist_paises{k}')),paises_graf()))})
      graficar(df = get(glue('Dist_paises{k}')),
               paises = paises_graf(),
               download = F,
               smooth = input[[glue('smooth_{k}')]])
      })
    
  })
  
  #paises dado comp
  
  lapply(k_vec,function(k){
    
    comp_graf <- reactive({input[[glue('paises_comp_{k}')]]    })
    output[[glue("plot_comp{k}")]] <- renderPlot({
      # output[[glue("plot{k}")]] <- renderPlotly({
      # ggplotly(graficar(get(glue('Dist_paises{k}')),paises_graf()))})
      graficar_dado_comp(df = get(glue('Dist_paises{k}')),
               comp = comp_graf(),
               n_paises = input[[glue('npaises_comp_{k}')]],
               download = F,
               smooth = input[[glue('smooth_comp_{k}')]])
    })
    
  })
  
  #download paises
  
  lapply(k_vec,function(k){
    
    paises_graf <- reactive({input[[glue('paises_{k}')]]    })
    # archivo <- reactive({make_filename(paises_graf(),k)})
    output[[glue("downloadPlot_{k}")]] <- downloadHandler(
      
      
      filename = function(){
        
        isos <- codigos_paises %>% filter(reporter %in% paises_graf()) %$% paste(rep_iso, collapse = '_')
        
        glue('graficoLDA_k{k}_{isos}.png')
      },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, 
               plot = graficar(get(glue('Dist_paises{k}')),paises_graf(), download = T,smooth = input[[glue('smooth_{k}')]]),
               scale=2,
               device = device)
      }
    )
  })
  
  
  #download paises by comp
  lapply(k_vec,function(k){
    
    comp_graf <- reactive({input[[glue('paises_comp_{k}')]]    })
    # archivo <- reactive({make_filename(paises_graf(),k)})
    output[[glue("downloadPlot_comp_{k}")]] <- downloadHandler(
      
      
      filename = function(){
        
        isos <- codigos_paises %>% filter(reporter %in% paises_graf()) %$% paste(rep_iso, collapse = '_')
        
        glue('graficoLDA_k{k}_{isos}.png')
      },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, 
               plot = graficar_dado_comp(get(glue('Dist_paises{k}')),
                                         comp_graf(),
                                         n_paises = input[[glue('npaises_comp_{k}')]],
                                         download = T,
                                         smooth = input[[glue('smooth_comp_{k}')]]),
               scale=2,
               device = device)
      }
    )
  })
  
}




##### RUN #####

shinyApp(ui, server)

