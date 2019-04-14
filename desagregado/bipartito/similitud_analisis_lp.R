#"Similitud analysis Serie larga

library(tidyverse)
library(ggvoronoi)
library(viridis)
library(ggthemes)
library(cluster)
library(igraph)
library(readxl)

set.seed(1234)

#data
similitud <- read_csv('results/similitud_LP_2016.csv',col_types = cols(SITC = col_character()))
clasificacion <-read_xlsx("../LDA/names/classifications_hidalgo.xlsx", sheet = 'sitc_product_id') %>% 
  rename(medioide=sitc_product_code)


#basic trasnform
M <- as.matrix(similitud[,-1])
#distance matrix
DM <- 1/M

###### Funciones ##########


# K medioids

plot_pam_giant_graph <- function(M,pam_clust,threshold = 0.5){
  
  #grafo
  adj_mat <- M
  #pongo un threshold
  adj_mat[adj_mat<threshold] <- 0
  
  g  <- graph_from_adjacency_matrix(adj_mat, weighted=TRUE,mode='undirected')
  
  graphs <- decompose.graph(g)
  comp_gigante <- graphs[[which.max(sapply(graphs, vcount))]]
  
  # Pongo solo la etiqueta de los medioides
  
  names <- V(comp_gigante)$name
  # names <- V(g)$name
  mediod_label <- names
  ind <- which(!mediod_label %in% pam_clust$medoids) 
  mediod_label[ind]<-NA
  V(comp_gigante)$label <- mediod_label
  # V(g)$label <- mediod_label
  
  colors <- tibble(cluster = unique(pam_clust$clustering), color = colorspace::rainbow_hcl(length(unique(pam_clust$clustering)),c = 100, l = 60, start = 0,alpha = 0.75))
  
  colors_df <- tibble(names) %>% 
    left_join(.,tibble(names = names(pam_clust$clustering),cluster = pam_clust$clustering)) %>% 
    left_join(colors)
  
  # V(g)$color <- colors_df$color
  V(comp_gigante)$color <- colors_df$color
  
  l <-layout_nicely(comp_gigante)
  # l <-layout_nicely(g)
  
  # plot(g,edge.arrow.size=.4,vertex.frame.color="#ffffff", edge.size = .1,
  #      vertex.label=V(g)$label, vertex.label.color="black",vertex.size = 5,
  #      vertex.label.cex=1.2,  layout=l)
  plot(comp_gigante,edge.arrow.size=.4,vertex.frame.color="#ffffff", edge.size = .1,
       vertex.label=V(comp_gigante)$label, vertex.label.color="black",vertex.size = 5,
       vertex.label.cex=1.2,  layout=l)
  
}

### Max spanning tree

plot_pam_max_span <- function(M,pam_clust,threshold = 0.5){
  
  #grafo
  adj_mat <- M
  #pongo un threshold
  # adj_mat[adj_mat<threshold] <- 0
  
  g  <- graph_from_adjacency_matrix(adj_mat, weighted=TRUE)
  
  g_mst <- mst(g)  
  # Pongo solo la etiqueta de los medioides
  
  names <- V(g_mst)$name
  mediod_label <- names
  ind <- which(!mediod_label %in% pam_clust$medoids) 
  mediod_label[ind]<-NA
  V(g_mst)$label <- mediod_label
  
  colors <- tibble(cluster = unique(pam_clust$clustering), color = colorspace::rainbow_hcl(length(unique(pam_clust$clustering)),c = 100, l = 60, start = 0,alpha = 0.75))
  
  colors_df <- tibble(names) %>% 
    left_join(.,tibble(names = names(pam_clust$clustering),cluster = pam_clust$clustering)) %>% 
    left_join(colors)
  
  V(g_mst)$color <- colors_df$color
  
  g_mst <-  decompose.graph(g_mst)[[1]]
  
  # l <-layout_nicely(g_mst)
  
  plot(g_mst,edge.arrow.size=.4,vertex.frame.color="#ffffff", edge.size = .1,
       vertex.label=V(g_mst)$label, vertex.label.color="black",vertex.size = 5,
       vertex.label.cex=1.2)#,  layout=l)
  
}


######### PAM

#K=2
pam_clust2 <- pam(DM,diss = TRUE,k=2)

png('results/pam2_gigant_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_giant_graph(M,pam_clust2,threshold = 0.35)
dev.off()

png('results/pam2_mst_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_max_span(M,pam_clust2)
dev.off()



# K=10
pam_clust10 <- pam(DM,diss = TRUE,k=10)

png('results/pam10_gigant_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_giant_graph(M,pam_clust10)
dev.off()

png('results/pam10_mst_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_max_span(M,pam_clust10)
dev.off()

#K=50
pam_clust50 <- pam(DM,diss = TRUE,k=50)

png('results/pam50_gigant_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_giant_graph(M,pam_clust50)
dev.off()

png('results/pam50_mst_lp.png',width = 6,height = 6, units = 'in', res = 300)
plot_pam_max_span(M,pam_clust50)
dev.off()