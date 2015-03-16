source("ga.r")

sol = init("greddy")[1:200]

trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), 
             move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c(),
             iterations=c(), total_eval=c(), pmutation=c(), pcrossover=c())

init_eval = subfitness(sol)
for(t in 1:500) {
  pcrossover=ceiling(runif(1,0,100))/100
  pmutation=ceiling(runif(1,0,100))/100
  
  for(i in 1:20) {
    control = list(maxiter=100, REPORT=0,TRACE=0, fn = subfitness, popsize=50, pmutation=pmutation,
                   pcrossover=pcrossover)
    sr = gen(sol,fn=subfitness,change=twoopt,distance=dist_exp,lower=NA,upper=NA,control, type="min")
    
    save(sr, file=paste("ga/results/santa_", ceiling(subfitness(sr$sol)),  ".RData", sep=""))
    
    trace = updatetrace(trace, control$maxiter, "ga", "twoopt", "dist_exp", 
                        subfitness(sr$sol), NA, NA, NA, NA, NA, init_eval, 
                        iterations=control$maxiter, subfitness(sr$sol), pmutation=pmutation, pcrossover=pcrossover)
  }
  
  savetrace(trace, path="ga/ga_stats.csv")

}