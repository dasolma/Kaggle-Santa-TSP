setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa.r")
library(ggplot2); 
library(animation); 
library(maps); 
library(plyr)

files <- Sys.glob(paste("cluster/*.RData", sep=""))
files <- sort(files, decreasing = TRUE)

create_image <- function(i){
  #print(current)
  load(files[i])
  g = show_sol(result$sol, colour=ifelse(i > 1, "red", "black"))
 

  if( i > 1 ) {
    load(files[i-1])
    g = g + geom_point(data=cities[result$sol,], colour="black") + geom_path(data=cities[result$sol,], colour="black") 
  }
  g
}

oopt <- animation::ani.options(interval = 0.1)

FUN2 <- function() {
  v = 1:length(files)
  lapply(v, function(i) {
    print(create_image(i))
    ani.pause()
  })
}


greddy_animate <- function() {
  v = (1:((150000)/2))*2
  lapply(v, function(i) {
    g = show_sol(init("greddy")[1:i]) 
    ggsave(g, file=paste('animations/greddy/img', sprintf("name_%07d", i),'.png', sep=""))
  })
 
}

#saveHTML(greddy_animate(), autoplay = FALSE, loop = FALSE, verbose = FALSE, outdir = "/animations/greddy/",
#         ani.height = 1200, ani.width = 1200,
#         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
