setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
library("GA")

gen <- function(par,fn,change,distance=function(x) 1,lower,upper,control, type="min",...)
{ 
  
  twoopt_mutation <- function(object, parent, ...) {
    parent <- as.vector(object@population[parent, ])
    s = twoopt(parent, dist_exp)
    return(s$sol)
  }
  
  santa_population <- function (object, ...) 
  {
    cat(fn(par), "\n")
    max <- length(par)
    population <- matrix(as.double(NA), nrow = object@popSize, 
                         ncol = max)
    
    for (i in 1:(object@popSize-1)) {
      aux = par
      if( runif(1,0,1) > 0.5 ) {
        for(j in 1:(length(par)*0.1)){ aux = near_opt(aux, dist_exp)$sol }
        population[i, ] <- aux
      }
      else {
        population[i, ] <- randomsol(par)
        #population[i, ] = par
      }
      population[i, ] <- randomsol(par)
    }

    population[object@popSize, ] = par
    return(population)
  }
  
  monitor <- function(obj) {
    curve(f, min, max, main = paste("iteration =", obj@iter), font.main = 1)
    points(obj@population, -obj@fitness, pch = 20, col = 2)
    rug(obj@population, col = 2)
    Sys.sleep(0.2)
  }
  
  r = ga(type = "permutation",
     fitness = function(...) {-fn(...)}, 
     population=santa_population,
     crossover=gaperm_pbxCrossover,
     mutation=twoopt_mutation,
     selection=gaperm_tourSelection,
     pmutation=control$pmutation,
     pcrossover=control$pmutation,
     min = c(1, 1), max = c(length(par), length(par)),
     popSize = control$popsize, maxiter = control$maxit, monitor = ifelse(control$REPORT>0, gaMonitor, NA),
     seed = proc.time()[3], parallel = FALSE)
  
  s = as.vector(r@solution[1,])
  cat(subfitness(s), "\n")
  
  return(list(sol=s,eval=subfitness(s)))

}
