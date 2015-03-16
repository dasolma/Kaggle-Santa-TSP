source("santa.r")
source("hill.r")
source("tabu.r")
source("sann.r")



equal_functions <- function(f,g)
  all( 
    capture.output(print(f)) ==
      capture.output(print(g))
  )
opnames = c("twoopt", "neighbor", "reduce_max_dist")
op = c(twoopt, neighbor, reduce_max_dist)
pop = c(1, 1, 1)

distnames = c("dist_exp", "dist_iden")
dist = c(dist_exp, dist_iden)
pdist = c(1, 1)

algname = c("hill", "sann", "tabu")
alg = c(hclimbing, sann, tabu)
palg = c(1, 1, 1)

L = c(4,10,20,50)
pL = c(1,1,1,1)

T = c(1, (1:10) * 2)
pT = rep(1,11)

alpha = c(0.55, 0.66, 0.75, 0.85, 0.95, 0.99)
palpha = rep(1,6)
tmax = 100

controls = list()

itmax = 1000
scorefactor = 1
saveit = 100 * itmax

totalit = 0

maxperturbationit = 100 * itmax
perturbationit = 0

sol = init("optima")
eval = subfitness(sol)

trace = list(init_fitness=c(), alg=c(), eval = c(), move = c(), move_control=c(), alpha = c(), T = c(), N = c(), L=c(), tmax=c() )
while(TRUE) {
  algo = select(algname, palg)
  move = select(opnames, pop)
  move_control = select(distnames, pdist)
  
  current_eval = subfitness(sol)
  
  if( algo == "hill" ) {
    cat("Executing hill-climbing alg\n")
    C = list(maxit=itmax,REPORT=0,TRACE=0)
    
    
    result = hclimbing(sol, subfitness, change=op[which(opnames==move)][[1]],
                       distance=dist[which(distnames==move_control)][[1]],
                       lower=rep(0,D), upper=rep(D,D), control=C, type="min")
    
    
    trace = updatetrace(totalit, algo, move, move_control, result$eval, tmax, NA, NA, NA, NA, current_eval)
  }
  
  if( algo == "sann") {
    cat("Executing sann alg\n")
    
    t = select(T, pT)
    a = select(alpha, palpha)
    C = list(maxit=itmax/tmax,REPORT=0,T=t, tmax=tmax, alpha=a, TRACE=0)
    result = sann(sol, subfitness, change=op[which(opnames==move)][[1]],
                  distance=dist[which(distnames==move_control)][[1]],
                  lower=rep(0,D), upper=rep(D,D), control=C)
    
    trace = updatetrace(totalit, algo, move, move_control, result$eval, tmax, t, NA, NA, a, current_eval)
    
    if(result$eval < eval) {
      pT = updatescore(pT, T, t, result$eval < eval, 1)
      palpha = updatescore(palpha, alpha, a, result$eval < eval, 1)
    }
    
  }
  
  if( algo == "tabu") {
    cat("Executing tabu alg\n")
    
    l = select(L, pL)
    n=min(itmax/100, 10)
    C = list(maxit=itmax/n,REPORT=0,L=l, N=n, TRACE=0)
    
    result = tabu(sol, subfitness, change=op[which(opnames==move)][[1]],
                  distance=dist[which(distnames==move_control)][[1]],
                  lower=rep(0,D), upper=rep(D,D), control=C)
    
    trace = updatetrace(totalit, algo, move, move_control, result$eval, tmax, NA, n, l, NA, current_eval)
    
    if(result$eval < eval) {
      pL = updatescore(pL, L, l, result$eval < eval, 1)        
    }
    
  }
  
  #update best
  if(result$eval < eval) {
    sol = result$sol
    eval = result$eval 
  }
  
  #update scores
  palg = updatescore(palg, algname, algo, result$eval < eval, 1)
  pop = updatescore(pop, opnames, move, result$eval < eval, 1)
  pdist = updatescore(pdist, distnames, move_control, result$eval < eval, 1)
  
  #update iteration count  
  totalit = totalit + itmax
  scorefactor = scorefactor + 0.1
  
  
  #perturbation
  if(result$eval >= current_eval) {
    perturbationit = perturbationit + tmax
  }
  else {
    perturbationit = 0
  }
  
  
  if(perturbationit >= maxperturbationit) {
    cat("Perturbing...")
    sol = perturbation(sol)
    perturbationit = 0
  }
  
  #save best
  if(totalit %% saveit == 0) {
    save(result, file=paste("iterate_search/santa_", ceiling(eval),  ".RData", sep=""))
  }
  
  cat("It:", totalit, " best: ", eval, " eval: ", result$eval, "\n")
  
  savetrace()
}
