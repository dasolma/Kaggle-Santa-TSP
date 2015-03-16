sann <- function(par,fn,change,distance=function(x) 1,lower,upper,control, cooling = "geom", type="min", ...)
{ 
  maxit = control$maxit
  T = control$T
  tmax = control$tmax
  fpar=fn(par,...)
  best = par
  fbest = fpar
  i = 1
  
  blist = c()
  bcurrent = c()
  lit = c()
  
  while( i < maxit )
  {
    for(j in 1:tmax) {
      
      par1=change(par,distance,lower,upper)
      
      if( is.list(par1) ) {
        inc = par1$inc
        
        if( is.na(inc) ) {
          #cat(par1)
          fpar1=fn(par1$sol,...)
        }
        else {
          par1 = par1$sol
          fpar1 = fpar + inc
        }
      }
      else {fpar1=fn(par1,...)}
      
     
      
      r = runif(1, 0, 1)
      p = exp ( ((fpar - fpar1) / T) )
      p = ifelse(is.na(p), 0, p)
      p = max(p, 0.0000000000001)
      
      #cat(fpar1, ",", fpar, ",", r, ",", p, "\n")
      
      if(control$REPORT>0 &&(i==1||i%%control$REPORT==0))
        cat("i:",i,"best:",fbest,"current:",fpar,"T:", T,"\n")
      
      if(control$TRACE>0 &&(i==1||i%%control$TRACE==0)) {
        blist = c(blist, fbest)
        bcurrent = c(bcurrent, fpar)
        lit = c(lit, i)
      }
      
      
      #cat("fpar:", fpar, " fpar1:", fpar1, "T:", T, " p:", exp ( ((fpar - fpar1) / T) ), "\n")
      
      if( fpar1 < fpar || r < p ) {
        par = par1
        fpar = fpar1
        
        #cat("accepted: ", fpar, "\n")
      }

      if (fpar1 < fbest) {
        best = par1
        fbest = fpar1 
      }
      
      i = i  + 1
      
    }
    
    if(cooling == "geom") { T = control$alpha * T }
    if(cooling == "") T = T / (log(i/tmax)*tmax+exp(1))
  }
  
  if(control$REPORT>=1) cat("best:",fbest,"\n")
  
  return(list(sol=best,eval=fbest,trace=data.frame(it=lit, best=blist, current=bcurrent)))
}


