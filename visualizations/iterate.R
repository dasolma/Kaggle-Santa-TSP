library(combinat)
setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/alg/santa_sann.R")
source("santa/alg/santa_tabu.R")
source("santa/alg/santa_hill.R")


save_results <- function() {
  results = data.frame(alg=results$alg, init=results$init, init_fitness=results$init_fitness, eval=results$eval, move=results$move, move_control=results$move_control, T=results$T,
                       N=results$N, L=results$L, tmax=results$tmax, alpha=results$alpha)
  
  write.csv(results, file="results/parameters_sann_stat.csv")
}


maxit = 100
report = 0
trace = 100

L=c(4,10,20,50)
N=c(maxit/2, maxit/4, maxit/10, 1)
T= (1:10) * 2
tmax=c(maxit/2, maxit/4, maxit/10)
alpha= (((15:25) * 4) - 1) / 100
sol = c("greddy")
opnames = c("twoopt", "neighbor", "reduce_max_dist", "reduce_max_dist2", "angle_opt", "near_opt")
op = c(twoopt, neighbor, reduce_max_dist, reduce_max_dist2, angle_opt, near_opt)
move_control = c(dist_exp, dist_iden)
save_result = FALSE
it_per_par = 100

results = list(init=c(), init_fitness=c(), alg=c(), eval = c(), move = c(), move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c() )
for( s in sol ) {
  
  init_fitness = fitness(init(s))
  for( move in opnames) {
    print( move)
    for( mc in move_control ) {
      
      
       cat("hill\n")
       #hill
       control=list(maxit=maxit,REPORT=report,TRACE=trace, SAVE=save_result)
       
       for(k in 1:it_per_par) {
         result = santa_hill(s, move=op[which(opnames==move)][[1]],  move_control=mc, control=control, sol=init(s)[1:200])
         
         results$init = c(results$init, s)
         results$alg = c(results$alg, "hill")
         results$move = c(results$move, move)
         results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
         results$eval = c(results$eval, result$eval)
         results$tmax = c(results$tmax, tmax)
         results$T = c(results$T, NA)
         results$N = c(results$N, NA)
         results$L = c(results$L, NA)
         results$init_fitness = c(results$init_fitness, init_fitness)
       }

      
      cat("sann\n")
      #sann
      for( a in alpha ) {
        for( t in T ) {
          for(tm in tmax) {
          
              control=list(maxit=ceiling(maxit/tm),REPORT=report,T=t, tmax=tm, alpha=a, TRACE=trace, SAVE=save_result)
            
              for(k in 1:it_per_par) {
                result = santa_sann(s, move=op[which(opnames==move)][[1]],  move_control=mc, control=control, sol=init(s)[1:200])
                
                results$init = c(results$init, s)
                results$alg = c(results$alg, "sann")
                results$move = c(results$move, move)
                results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
                results$eval = c(results$eval, result$eval)
                results$tmax = c(results$tmax, tmax)
                results$T = c(results$T, t)
                results$N = c(results$N, NA)
                results$L = c(results$L, NA)
                results$alpha= c(results$alpha, a)
                results$init_fitness = c(results$init_fitness, init_fitness)
                
     
              }
          
          }
        
        }
        
      }

       cat("tabu\n")
       #tabu
       for( l in L ) {
         for( n in N ) {
           
           control=list(maxit=ceiling(maxit/n),REPORT=report,L=l, N=n, TRACE=trace, SAVE=save_result)
           
           for(k in 1:it_per_par) {
             result = santa_tabu(init_type=s, move=op[which(opnames==move)][[1]],  move_control=mc, control=control, sol=init(s)[1:200])
             
             results$init = c(results$init, s)
             results$alg = c(results$alg, "tabu")
             results$move = c(results$move, move)
             results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
             results$eval = c(results$eval, result$eval)
             results$tmax = c(results$tmax, tmax)
             results$T = c(results$T, NA)
             results$N = c(results$N, n)
             results$L = c(results$L, l)
             results$init_fitness = c(results$init_fitness, init_fitness)
 
           }
           
           
         }
       }
        
      cat("Grabando...\n")
      save_results()
    }
  }
}

#save_results()



