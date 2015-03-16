library(foreach)
library(doSNOW)
library(ggplot2)
cores <- 8
registerDoSNOW(makeCluster(cores, type = "SOCK"))

setwd("~/Documentos/projets/kaggle/santa_tsp")


cities <- read.csv("santa_cities.csv")

randomsol <- function() {
  sample(1:nrow(cities), nrow(cities))
}

greddysol <- function() {
  s <- 1:nrow(cities)  
  #s <- 1:10
  D <- length(s)
  for(i in 2:1000) {
    x1 = cities$x[s[i:D]]
    y1 = cities$y[s[i:D]]
    d  = sqrt((x1 - cities$x[s[i-1]])^2 + (y1 - cities$y[s[i-1]])^2)
    
    j <- which.min(d) + i - 1
    aux = s[i]
    s[i] = s[j]
    s[j] = aux
    
    cat(i, "\n")
  }

}

hclimbing <- function(par,fn,change,lower,upper,control, type="min",...)
{ 
  fpar=fn(par,...)
  for(i in 1:control$maxit)
  {
    par1=change(par,lower,upper)
    
    #
    if( length(par1) > length(par) ) {
      inc = par1[length(par1)]
     
      par1 = par1[-length(par1)]
      fpar1 = fpar + inc
    }
    else {fpar1=fn(par1,...)}
    
    if(control$REPORT>0 &&(i==1||i%%control$REPORT==0))
      cat("i:",i,"f:",fpar,"f:",fpar1,"\n")
    if(
      (type=="min" && fpar1<fpar)
      || (type=="max" && fpar1>fpar)) { par=par1;fpar=fpar1 }
  }
  if(control$REPORT>=1) cat("best:",fpar,"\n")
  return(list(sol=par,eval=fpar))
}


pfitness <- function(sol) {
  subfitness <- function(sol, cities) {
    x1 <- cities$x[sol[1:length(sol)-1]]
    y1 <- cities$y[sol[1:length(sol)-1]]
    x2 <- cities$x[sol[2:length(sol)]]
    y2 <- cities$y[sol[2:length(sol)]]
    sum(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }
  
  p1 <- sol[1:(nrow(cities)/2)]
  p2 <- sol[(nrow(cities)/2)+1:nrow(cities)]
  c <- cities
  
  n <- ceiling(length(p1) /  cores)
  r <- split(p1, ceiling(seq_along(p1)/n))
  
  f1 <- foreach(i = 1:cores, .combine="sum") %dopar% subfitness(r[[i]], c)
  
  r <- split(p2, ceiling(seq_along(p2)/n))
  f2 <- foreach(i = 1:cores, .combine="sum") %dopar% subfitness(r[[i]], c)
  
  return(max(f1,f2))
}

sfitness <- function(sol) {
  p1 <- sol[1:(nrow(cities)/2)]
  p2 <- sol[(nrow(cities)/2)+1:nrow(cities)]
  
  f1 <- subfitness(p1)
  f2 <- subfitness(p2)
  
  return(max(f1,f2))
}

getInterDistances <- function(sol, i) {
  d = 0
  D = length(sol)
  if(i > 1 && i < D) { ss = (i-1):(i+1) }
  else {
    if(i>1) { ss = (i-1):i}
    else {ss = (i):(i+1)}
  }
  subfitness(sol[ss])
}

subfitness <- function(sol) {
  x1 <- cities$x[sol[1:length(sol)-1]]
  y1 <- cities$y[sol[1:length(sol)-1]]
  x2 <- cities$x[sol[2:length(sol)]]
  y2 <- cities$y[sol[2:length(sol)]]
  sum(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

neighbor <- function(par,lower,upper) {
  D=length(par) # dimension
  
  #interchange
  i <- sample(1:D, 1)
  j <- ifelse(i==D,1,i+1)
  
  inc = getInterDistances(par, i) + getInterDistances(par, j)
  
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  inc = getInterDistances(par, i) + getInterDistances(par, j) - inc
  
  
  c(par, inc)
}

load("greddy.RData")
sol <- randomsol()
sol <- greddy_sol
D=length(sol)
C=list(maxit=100000,REPORT=100) # maximum of 10 iterations
santa_100000it_ls <- hclimbing(sol,subfitness,change=neighbor,lower=rep(0,D),upper=rep(D,D), control=C, type="min")

save(santa_100000it_ls, "santa_100000it_ls.RData")

