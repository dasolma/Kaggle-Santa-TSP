

setwd("~/Documentos/projets/kaggle/santa_tsp")

cities <- read.csv("santa_cities.csv")

greddysol <- function() {
  s <- 1:nrow(cities)  
  #s <- 1:10
  D <- length(s)
  for(i in 2:D) {
    x1 = cities$x[s[i:D]]
    y1 = cities$y[s[i:D]]
    d  = sqrt((x1 - cities$x[s[i-1]])^2 + (y1 - cities$y[s[i-1]])^2)
    
    j <- which.min(d) + i - 1
    aux = s[i]
    s[i] = s[j]
    s[j] = aux
    
    cat(i, "\n")
  }
  
  s
}

greddy_sol <- greddysol()

save(greddy_sol, file="greddy.RData")


load("greddy.RData")
sfitness(greddy_sol)

p <- ggplot(cities, aes(x, y))
p + geom_line()