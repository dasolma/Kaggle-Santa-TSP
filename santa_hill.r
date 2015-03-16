
setwd("~/Documentos/projets/kaggle/santa_tsp")
source("hill.r")
source("santa.r")


santa_hill <- function(init_type="greddy", move=neighbor, move_control=dist_exp, control, sol=NA) {

  sol = init(init_type, sol)
  D=length(sol)
  C=control # maximum of 10 iterations
  result <- hclimbing(sol,subfitness,change=move,distance=move_control,lower=rep(0,D),upper=rep(D,D), control=C, type="min")
  
  result$control = C
  result$movefun = as.character(substitute(move))
  result$movectrl = as.character(substitute(move_control))
  result$init = init_type
  
  if( control$SAVE )
    save(result, file=paste("hill/santa_", ceiling(result$eval),  "_hill.RData", sep=""))

  result
}

#result = santa_hill("otra", move=reduce_max_dist, move_control=dist_exp, list(maxit=10000,REPORT=100,TRACE=100))

#library(reshape2)
#dt <-melt(result$trace,id.vars="it")

#p <- ggplot(dt, aes(x=it, y=value, colour=variable)) 
#p + geom_line()