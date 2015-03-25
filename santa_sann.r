setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("sann.r")
source("santa.r")

santa_sann <- function(init_type="greddy", move=neighbor, move_control=dist_exp, control, sol=NA) {
  
  if( is.na(sol) ) sol = init(init_type)
  D=length(sol)
  C=control # maximum of 10 iterations

  result <- sann(sol,subfitness,change=move,distance=move_control, lower=rep(0,D),upper=rep(D,D), control=C, cooling="geom")
    
  result$control = C
  result$movefun = as.character(substitute(move))
  result$movectrl = as.character(substitute(move_control))
  result$init = init_type
  
  if( control$SAVE )
    save(result, file=paste("sann/santa_", C$T, "T_", ceiling(result$eval),  "_sann.RData", sep=""))

  result
}

#result = santa_sann("greddy", move=twoopt, list(maxit=1000,REPORT=100,T=10, tmax=100, alpha=0.99, TRACE=100))

#subfitness(santa_sann$sol)


#p <- ggplot(cities[sol,], aes(x=x, y=y)) 
#p + geom_point(size = 1, colour="red")  + geom_path(colour="black") 
#ggsave(g, file="santa.png")