
aco <- function(par,fn,change,distance=function(x) 1,lower,upper,control, type="min", ...) {
  D = length(par)
  
  maxit = control$maxit
  it = 0
  k = 0
  
  fbest=fn(par, ...)
  best = par
  #par = sample(par, D)
  
  #trace init
  trace.blist = c()
  trace.bcurrent = c()
  trace.lit = c()
  
  if(control$REPORT>0 )
    cat("Creating ants...\n")
  
  #initialize pheromone trains
  pheromone = matrix(rep(1/D, D), nrow=D, ncol=D)
  distances = matrix(rep(0, D), nrow=D, ncol=D)
  for(i in 1:(D-1)) {
    for(j in (i+1):D) {
      if( i != j) {
        distances[i,j] = 1/fn(c(par[i], par[j]))
        distances[j,i] = distances[i,j]  
      }
      else { distances[i,j] = 0 }
    }
  }
  
  if(control$REPORT>0 )
    cat("Ants leaving the nest...\n")
  
  while( it < maxit ) {
    cbest.sol = c()
    cbest.fitness = Inf
    
    # solution construction
    for(ant in 1:control$ants) {
      #select the start city
      i = 1
      if( control$attach_extremes == FALSE) { i = ceiling(runif(1,1,D)) }
      s = c(par[i])
      
      for(k in 2:ifelse(control$attach_extremes == FALSE, D, D-1)) {
        
        #select the next city by probablity
        prob = pheromone[i,]
        yet_selected = par %in% s
        
        
        prob = prob^control$alpha * distances[i,]^control$beta
        prob[prob == Inf] = 0
        prob[which(yet_selected)] = 0
        if( control$attach_extremes == TRUE) { prob[D] = 0 }
        prob = cumsum(prob /  sum(prob))
        prob[yet_selected] = -1
        
        p  = runif(1,0,1)
        j = which(p < prob)[1]
        
       
        #add to the current solution
        s = c(s, par[j])
        i = j
   
      
      }
      
      if( control$attach_extremes == TRUE) { s = c(s, par[D]) }
      #update current best
      f = fn(s, ...)
      if( f < cbest.fitness  ) {
        cbest.fitness = f
        cbest.sol = s
      }
      
    }
    
    #update the pheromone trails
    for(k in 2:D) {
      i = which(par == cbest.sol[k-1])  
      j = which(par == cbest.sol[k])  
      
      pheromone[i,j] = pheromone[i,j] + (1/cbest.fitness)
      
    }
    
    #evaporation
    pheromone = pheromone * (1 - control$reduction_rate)
    it = it + 1
    
    #update best
    #update current best
    if( cbest.fitness < fbest ) {
      fbest = cbest.fitness
      best = cbest.sol

    }

    
    if(control$REPORT>0 &&(it==1||it%%control$REPORT==0))
      cat("it:",it,"best:",fbest,"current:",cbest.fitness,"\n")
    
   
    
  }
  
  return(list(sol=best,eval=fbest, trace=data.frame(it=trace.lit, best=trace.blist, current=trace.bcurrent)))
  
}