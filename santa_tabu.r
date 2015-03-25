setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa.r")
source("tabu.r")

santa_tabu <- function(init_type="greddy", move=neighbor, move_control=dist_exp, control, sol = NA) {
  
  if( is.na(sol) ) sol = init(init_type)
  D=length(sol)
  C=control 
  
  result <- tabu(sol,subfitness,change=move,distance=move_control, lower=rep(0,D),upper=rep(D,D), control=C)
  
  result$control = C
  result$movefun = as.character(substitute(move))
  result$movectrl = as.character(substitute(move_control))
  result$init = init_type
  
  if( control$SAVE )
    save(result, file=paste("tabu/santa_", C$T, "T_", ceiling(result$eval),  "_sann.RData", sep=""))
  
  result

}

#result = santa_tabu(init_type="greddy", move=neighbor, control=list(maxit=10,REPORT=100,L=4, N=100, TRACE=100))
