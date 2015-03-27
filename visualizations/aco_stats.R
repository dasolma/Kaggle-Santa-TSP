setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("alg/santa.R")
source("alg/aco.R")


sol = init("greddy")[1:200]

trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), 
             move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c(),
             iterations=c(), total_eval=c(), pmutation=c(), pcrossover=c())

init_eval = fitness(sol)
for(t in 1:500) {
  alpha=ceiling(runif(1,0,100))/100
  beta=ceiling(runif(1,0,100))/100
  
  for(i in 1:20) {
    control = list(maxiter=100, REPORT=0,TRACE=0, fn = fitness, ants=5, reduction_rate=0.01, 
                   alpha=alpha, beta=beta, attach_extremes=FALSE)

    sr = aco(sol,fn=fitness,change=twoopt,distance=dist_exp,lower=NA,upper=NA,control, type="min")
    
    cat(sr$eval, "\n")
    
    trace = updatetrace(trace, control$maxiter, "aco", "twoopt", "dist_exp", 
                        fitness(sr$sol), NA, NA, NA, beta, alpha, init_eval, 
                        iterations=control$maxiter, fitness(sr$sol), pmutation=NA, pcrossover=NA)
  }
  
  savetrace(trace, path="results/aco/ga_stats.csv")
  
}