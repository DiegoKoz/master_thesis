library(pdftools)
library(tidyverse)
library(tm)
library(wordcloud2)
# library(wordcloud)
library(memoise)
library(tidyverse)
library(magrittr)
# cachea resultados y no recalcula todo
getTermMatrix <- memoise(function(text) {
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords(kind = "es"),'the', 'and', 'red','-','’','fig'))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})


text <- pdf_text('Downloads/tesis.pdf')

text <- paste(text, collapse = '\n')

text <- str_remove_all(text, '\n')


dtm <- getTermMatrix(text)

vv <- tibble(word = names(dtm), freq = dtm)

vv <- vv %>% 
  filter(freq>25)
# %>% 
#   top_n(.,n=input$max, wt = freq)

wordcloud2(vv, shuffle = FALSE,size = 0.5)




