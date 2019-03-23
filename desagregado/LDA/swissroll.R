library(spe)
library(plotly)
data(swissroll)

plot_ly(swissroll, x = ~x, y = ~y, z = ~z, color = ~z, 
  marker = list(symbol = 'circle', sizemode = 'diameter',size=2)) %>% 
  layout(showlegend = FALSE) %>% 
  hide_colorbar() 
