setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
source("ga.r")

totalit = 0
tmax = 100
itmax = 1000

#result = list(sol = init("greddy"))
#result = list(sol=greddysol_directional(div = 1000, dir = "H"))
result = list(sol = init("optima"))



trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c() )
while(TRUE) {
  clusters = 3000
  clu_size = length(result$sol) / clusters
  sol = result$sol
  cat("-------------------------------------\n")
  cat("Total fitness: ", subfitness(sol), "\n")
  cat("-------------------------------------\n")
  for(i in 1:clusters) {
    #indexes = (1:clu_size) + clu_size * (i-1)
    set.seed(proc.time()[3])
    j = sample(1:(length(sol)-clu_size), 1)
  
    indexes = j:(j+clu_size-1)
    subsol = sol[indexes]
    #show_sol(subsol)
    
    C = list(maxit=itmax,REPORT=0,TRACE=0)
    
    init_eval = subfitness(subsol)
    sr = gen(subsol, subfitness, change=NA,
                   distance=dist_exp,
                   lower=rep(0,D), upper=rep(D,D), control=C, type="min")
    
    totalit = totalit + itmax
    trace = updatetrace(totalit, "ga", "", NA, sr$eval, tmax, NA, NA, NA, NA, init_eval)
    
    cat("Improving:",  subfitness(subsol)-sr$eval, "\n")
    if( anyDuplicated(sr$sol) > 0){ cat("Duplicates found\n")}
    if( sr$eval < subfitness(subsol) && anyDuplicated(sr$sol) == 0 ) {
      sol[indexes] = sr$sol
    }
    cat("Total fitness: ", subfitness(sol), "\n")
  }
  result$sol = sol
  result$eval = subfitness(sol)
  save(result, file=paste("cluster/santa_", ceiling(result$eval),  ".RData", sep=""))
  savetrace(path="ga/ga_stats.csv")
}