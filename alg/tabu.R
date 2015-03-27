library("digest")

m_tabu = ((1:1000)/1000)

hashcode <- function(sol) {
  sum(sol*m_tabu)
}

tabu <- function(par,fn,change,distance=function(x) 1,lower,upper,control, type="min",...) {
  
  maxit = control$maxit
  L = control$L
  N = control$N
  tabuList = c()
  i = 0
  k = 0
  
  fbest=fn(par, ...)
  best = par
  
  blist = c()
  bcurrent = c()
  lit = c()
  
  while( i < maxit ) {
    interBest = best
    finterBest = fbest
    for(j in 1:N) {
      
      par1 = change(best, distance)
      k = k + 1
      
      if( is.list(par1) ) { 
        hsol = hashcode(par1$sol)
        inc = par1$inc
        par1 = par1$sol
        fpar1 = fbest + inc
      }
      else {
        fpar1 = fn(par1)
      }
      
      if( !(hsol %in% tabuList) && (is.na(finterBest) || fpar1 < finterBest ) ) { 
        finterBest = fpar1
        interBest = par1
      }
      
      if(control$REPORT>0 &&(k==1||k%%control$REPORT==0))
        cat("i:",k,"best:",fbest,"current:",finterBest,"\n")
      
      if(control$TRACE>0 &&(k==1||k%%control$TRACE==0)) {
        blist = c(blist, fbest)
        bcurrent = c(bcurrent, finterBest)
        lit = c(lit, k)
      }
    }
      
    if( finterBest < fbest ) {
      #enqueue into tabuList
      tabuList = c(tabuList, hashcode(interBest)) 
      #remove oldest element
      if( length(tabuList) > L ) tabuList = tabuList[length(tabuList) - L:length(tabuList)]
      
      #set the best solution
      fbest = finterBest
      best = interBest
      
      #cat("Tabu List:", tabuList)
    }
      
    i = i + 1 
    
  }
  
  return(list(sol=best,eval=fbest, trace=data.frame(it=lit, best=blist, current=bcurrent)))
  
}