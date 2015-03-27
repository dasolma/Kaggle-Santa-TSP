setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")


sol = init("second")
other_sol <- init("optima")
DO = length(other_sol)
DAUX = as.numeric(max(c(other_sol)))

dfos = c(other_sol[1:(DO-1)] * DAUX)  + c(other_sol[2:DO])
best = sol
while(TRUE) {
  n = ceiling(runif(1,5,200))
  
  ns = max_subtour(sol, n)
  
  #sol = ns
  cat("n:", n, " fitness:", optional_fitness(sol), "\n")
  if(optional_fitness(best) > optional_fitness(ns)) {
    best = ns
    sol = ns
    cat("n:", n, " fitness:", optional_fitness(best), "\n")
    
    save(best, file=paste("heuristic/santa_", optional_fitness(best),  ".RData", sep=""))
  }
}