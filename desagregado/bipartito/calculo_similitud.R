library(tidyverse)
library(countrycode)

countrycode_data <- codelist %>% select(country.name.en, cldr.short.es_ar, cldr.short.en,iso3c,region,continent)


similarity <- function(RCA){
  
  cualitative_RCA <- RCA %>% 
    mutate(RCA = as.integer(case_when(RCA > 1 ~ 1,
                                      RCA <= 1 ~ 0)))
  w <- cualitative_RCA %>% spread(., reporter,RCA,fill = 0) %>% 
    ungroup() 
  
  SITC <- w$SITC 
  mat <- as.matrix(w[,-1])
  v <- mat %*% t(mat)                                   
  diag(v) <- 0                                      
  dimnames(v) <- list(SITC, SITC) 
  totales <- rowSums(w[,-1])
  probabilities <- v/totales
  
  symmetric_proba <- symmetric_max(probabilities)
  return(symmetric_proba)   
}




RCA <- read_delim("results/RCA_mundo4d.txt",delim = ",")

symmetric_proba <- similarity(RCA = RCA)
write_csv(data.frame(symmetric_proba),"results/similitud_4d.csv")