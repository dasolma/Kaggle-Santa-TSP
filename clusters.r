setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
source("hill.r")
set.seed(proc.time()[3])


fn = optional_fitness

totalit = 0
tmax = 100
itmax = 1000

opnames = c("twoopt", "reduce_max_dist", "reduce_max_dist2", "angle_opt", "near_opt", 
            "optional_opt", "optional_angle_opt", "optional_near_opt", "optional_reduce_max_distance2_opt")
op = c(twoopt, reduce_max_dist, reduce_max_dist2, angle_opt, near_opt, optional_opt,
       optional_angle_opt,optional_near_opt, optional_reduce_max_distance2_opt)
pop = c(0, 0, 0, 0, 0, 1, 1, 1, 1)
opitfactor = c(1,0.1,0.1,0.1,0.1, 1, 1, 1, 0.1)

result = list(sol = init("other"))
other = init("optima")
other_sol <- other
#result = list(sol = create_optional(other_sol))
result$eval = fn(result$sol)

#result = list(sol=greddysol_directional(div = 1000, dir = "H"))
trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c() )
  while(TRUE) {
    clusters = sample(c(100, 200, 300, 50), 1)
    clu_size = length(result$sol) / clusters
    sol = result$sol
    cat("-------------------------------------\n")
    cat("Total fitness: ", fn(sol), "\n")
    cat("-------------------------------------\n")
    cat("Cluster size: ", clu_size, "\n")
    for(i in 1:clusters) {
      indexes = (1:clu_size) + (clu_size * (i-1))
      #i = sample(1:(length(sol)-clu_size), 1)
      #indexes = i:(i+clu_size-1)
      subsol = sol[indexes]
      
      #show_sol(subsol)
      
      
      init_eval = fn(subsol)
      move = select(opnames, pop)
      cat("op:", move, "\n")
      it = itmax * opitfactor[which(opnames==move)][[1]]
      cat("it: ", it, "\n")
      C = list(maxit=it,REPORT=0,TRACE=0)
    
      
      
      hasimproving = TRUE
      while(hasimproving) {
        other_sol <- other[indexes]
        sr = hclimbing(subsol, fn, change=op[which(opnames==move)][[1]],
                                   distance=dist_exp,
                                    lower=rep(0,D), upper=rep(D,D), control=C, type="min")
        
        
        totalit = totalit + it
    
        cat("i: ", i, "\n")
        trace = updatetrace(totalit, "hill", move, move, sr$eval, tmax, NA, NA, NA, NA, init_eval)
        
        auxsol = sol
        auxsol[indexes] = sr$sol
        other_sol <- other
        cat("Improving:",  result$eval-fn(auxsol), "\n")
        if( anyDuplicated(sr$sol) > 0){ cat("Duplicates found\n")}
        thereis_impr =  (fn(auxsol) <  result$eval && anyDuplicated(sr$sol) == 0 )
        if( thereis_impr ) {
          sol[indexes] = sr$sol
          subsol = sr$sol
          result$sol = sol
          result$eval = fn(sol)
        }
        else { hasimproving = FALSE }
        
        pop = updatescore(pop, opnames, move, thereis_impr, 1)
        cat(pop, "\n")
        cat("Total fitness: ", fn(sol), "\n")
      }
    }
    
    #if( fn(sol) < (result$eval-1000)  ) {
      result$sol = sol
      result$eval = fn(sol)
      #save(result, file=paste("other/santa_", ceiling(result$eval),  ".RData", sep=""))
    #}
    #savetrace(path="hill/hill_rmd2_stats.csv")
  }

#for(i in 1:1000) { 
#  inc = reduce_max_dist2(sol, dist_exp)$inc
#  if(inc < 0) cat(inc, "\n")
#}