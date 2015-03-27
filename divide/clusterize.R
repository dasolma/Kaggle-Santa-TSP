source("santa/santa.R")
set.seed(proc.time()[3])

opnames = c("twoopt", "reduce_max_dist", "reduce_max_dist2", "angle_opt", "near_opt", 
            "second_opt", "second_angle_opt", "second_near_opt", "second_reduce_max_distance3_opt")
op = c(twoopt, reduce_max_dist, reduce_max_dist2, angle_opt, near_opt, second_opt,
       second_angle_opt,second_near_opt, second_reduce_max_dist3_opt)

opitfactor = c(1,0.1,0.1,0.1,0.1, 1, 1, 1, 0.1)
other_sol <- NA
clusterize <- function(init_sol, alg, control, pop, clu_method="linear", sol_path =NA, second_sol=NA, trace_file=NA, trace_function=NA) {

  fn = control$fn
  totalit = 0
  tmax = 100
  
  result = list(sol = init_sol)
  other = ifelse( is.na(second_sol), init_sol, second_sol)
  other_sol <- other
  result$eval = fn(result$sol)
  
  #configure trace
  exist_trace_file = FALSE
  if( !is.na(trace_file) ) { exist_trace_file = file.exists(trace_file) }
  
  #trace control
  if( exist_trace_file ) {
    trace = read.csv(trace_file)
    trace = list(init_fitness=trace$init_fitness, alg=trace$alg, eval = trace$eva, move = trace$move, 
                 move_control=trace$move_control, alpha = trace$alpha, T = trace$T, N = trace$N, L=trace$L, tmax=trace$tmax,
                 iterations=trace$iterations, total_eval=trace$total_eval)
  }
  else {
    trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), 
                 move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c(),
                 iterations=c(), total_eval=c())
  }
  
  finish = FALSE
  
  while(!finish) {
    start_eval= result$eval
    clusters = sample(control$clusters_size, 1)
    clu_size = length(result$sol) / clusters
    sol = result$sol
    cat("-------------------------------------\n")
    cat("Total fitness: ", fn(sol), "\n")
    cat("\nCluster size: ", clu_size, "\n")
    cat("-------------------------------------\n")
    
    for(i in 1:clusters) {
      #select cluster
      if( clu_method != "linear" ) { 
        i = sample(1:(length(sol)-clu_size), 1) 
        indexes = i:(i+clu_size)
      } else {
        indexes = (1:clu_size) + (clu_size * (i-1))
      }
      cat("Cluster start: ", i, "\n")
      
      

      
      subsol = sol[indexes]
      subsol = subsol[!is.na(subsol)]
   
      #select operator
      init_eval = fn(subsol)
      move = select(opnames, pop)
      cat("op:", move, "\n")
      
      #update itmax
      control$maxit = control$itmax * opitfactor[which(opnames==move)][[1]]
      cat("it: ", control$maxit, "\n")
    
      
      hasimproving = TRUE
      while(hasimproving) {
        if( !is.na(second_sol) ) {other_sol <- other[indexes]}

        
        sr = alg(subsol, control$fn, change=op[which(opnames==move)][[1]],
                       distance=dist_exp,
                       lower=rep(0,D), upper=rep(D,D), control=control, type="min")
        
        
        
        totalit = totalit + control$maxit
        
        cat("Cluster: ", i, "\n")
        
        
        
        auxsol = sol
        auxsol[indexes] = sr$sol
        
        subimp = fn(subsol) - fn(sr$sol)
        cat("Cluster Improving:",  subimp, "\n")
        if( !is.na(second_sol) ) {other_sol <- other}
        
        imp = result$eval-fn(auxsol)
        cat("Improving:",  imp, "\n")
        
        #if (subimp > 0 & imp < 0 ) {
        #  return(list(inisubsol = subsol, finalsubsol=sr$sol, ini=sol, final=auxsol, indexes=indexes))
        #}
        
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
        if( length(pop[pop!=0]) == 1) pop = ifelse(pop != 0, 1, 0)
        cat(pop, "\n")
        cat("Total fitness: ", fn(sol), "\n\n")
        
        #trace control
        if( !is.na(trace_file) ) {  
          if( !is.na(trace_function)) {
            #trace control
            control$totalit = totalit
            control$move = move
            control$eval = sr$eval
            control$tmax = tmax
            control$init_eval = init_eval
            control$total_eval = result$eval
            trace = trace_function(control, trace)
          }
          else {
            trace = updatetrace(trace, totalit, "hill", move, move, 
                                sr$eval, tmax, NA, NA, NA, NA, init_eval, control$itmax, result$eval)
          }
          
        }
      }
    }
    

    #stop criterion
    cat(start_eval, "\n", fn(sol))
 
    finish = fn(sol) >= start_eval 
    
    #save data
    result$sol = sol
    result$eval = fn(sol)
    cat(sol_path)
    if( !is.na(sol_path) ) {  
      save(result, file=paste(sol_path, "santa_", ceiling(result$eval),  ".RData", sep=""))
    }
    if( !is.na(trace_file) ) {  
      savetrace(trace, path=trace_file)
    }
  }

  result
}


