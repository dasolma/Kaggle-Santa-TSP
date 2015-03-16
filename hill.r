
hclimbing <- function(par,fn,change,distance=function(x) 1,lower,upper,control, type="min",...)
{ 
  fpar=fn(par,...)
  blist = c()
  bcurrent = c()
  lit = c()
  for(i in 1:control$maxit)
  {
    par1=change(par, distance)
    
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
    
    if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) {
      cat("i:",i,"f:",fpar,"f:",fpar1,"\n")
    }
    
    if(control$TRACE>0 &&(i==1||i%%control$TRACE==0)) {
      blist = c(blist, fpar)
      bcurrent = c(bcurrent, fpar1)
      lit = c(lit, i)
    }
    
    if(
      (type=="min" && fpar1<fpar)
      || (type=="max" && fpar1>fpar)) { par=par1;fpar=fpar1 }
  }
  if(control$REPORT>=1) cat("best:",fpar,"\n")
  return(list(sol=par,eval=fpar,trace=data.frame(it=lit, best=blist, current=bcurrent)))
}
