library(combinat)
source("santa_sann.r")
source("santa_tabu.r")
source("santa_hill.r")

save_results <- function() {
  results = data.frame(alg=results$alg, init=results$init, init_fitness=results$init_fitness, eval=results$eval, move=results$move, move_control=results$move_control, T=results$T,
                       N=results$N, L=results$L, tmax=results$tmax)
  
  write.csv(results, file="iterate.csv")
}


maxit = 1000
report = 0
trace = 100

L=c(4,10,20,50)
N=min(maxit/100, 10)
T= (1:10) * 2
tmax=100
alpha= (((15:25) * 4) - 1) / 100
sol = c("greddy", "random", "optima")
move = c(twoopt, neighbor)
move_control = c(dist_exp, dist_iden)
save_result = FALSE


results = list(init=c(), init_fitness=c(), alg=c(), eval = c(), move = c(), move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c() )
for( s in sol ) {
  
  init_fitness = subfitness(init(s))
  for( m in move) {
    
    for( mc in move_control ) {
      
      #hill

      
      control=list(maxit=maxit,REPORT=report,tmax=tmax, TRACE=trace, SAVE=save_result)
      
      result = santa_hill(s, move=m,  move_control=mc, control=control)
      
      results$init = c(results$init, s)
      results$alg = c(results$alg, "hill")
      results$move = c(results$move, ifelse(identical(m,twoopt),"twoopt", "simple"))
      results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
      results$eval = c(results$eval, result$eval)
      results$tmax = c(results$tmax, tmax)
      results$T = c(results$T, NA)
      results$N = c(results$N, NA)
      results$L = c(results$L, NA)
      results$init_fitness = c(results$init_fitness, init_fitness)
      
      print(result$eval)
      
          
      

      #sann
      for( a in alpha ) {
        for( t in T ) {
          
          control=list(maxit=maxit,REPORT=report,T=t, tmax=tmax, alpha=a, TRACE=trace, SAVE=save_result)
          
          result = santa_sann(s, move=m,  move_control=mc, control=control)
          
          results$init = c(results$init, s)
          results$alg = c(results$alg, "sann")
          results$move = c(results$move, ifelse(identical(m,twoopt),"twoopt", "simple"))
          results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
          results$eval = c(results$eval, result$eval)
          results$tmax = c(results$tmax, tmax)
          results$T = c(results$T, t)
          results$N = c(results$N, NA)
          results$L = c(results$L, NA)
          results$init_fitness = c(results$init_fitness, init_fitness)
          
          print(result$eval)
          
        
        }
      }

      #tabu
      for( l in L ) {
        for( n in N ) {
          
          control=list(maxit=ceiling(maxit/N),REPORT=report,L=l, N=n, TRACE=trace, SAVE=save_result)
          
          result = santa_tabu(init_type=s, move=m,  move_control=mc, control=control)
          
          results$init = c(results$init, s)
          results$alg = c(results$alg, "tabu")
          results$move = c(results$move, ifelse(identical(m,twoopt),"twoopt", "simple"))
          results$move_control = c(results$move_control, ifelse(identical(mc, dist_exp),"exp", "ident"))
          results$eval = c(results$eval, result$eval)
          results$tmax = c(results$tmax, tmax)
          results$T = c(results$T, NA)
          results$N = c(results$N, n)
          results$L = c(results$L, l)
          results$init_fitness = c(results$init_fitness, init_fitness)
          
          print(result$eval)
          
          
        }
      }
  
    }
  }
}

save_results()



