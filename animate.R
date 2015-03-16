setwd("~/Documentos/projets/kaggle/santa_tsp")
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

saveHTML(FUN2(), autoplay = FALSE, loop = FALSE, verbose = FALSE, outdir = "images/animate/new",
         ani.height = 1200, ani.width = 1200,
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")

