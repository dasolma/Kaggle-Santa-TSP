library(foreach)
library(doSNOW)
library(ggplot2)
#cores <- 8
#registerDoSNOW(makeCluster(cores, type = "SOCK"))

setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")

cities <- read.csv("santa_cities.csv")


randomsol <- function(par=NA) {
  if( is.na(par)) {sample(1:nrow(cities), nrow(cities))}
  else { sample(par, length(par)) }
}

greddysol <- function(s = NA, attach_left = FALSE, D=length(s)) {
  if( is.na(s) ) { s <- 1:nrow(cities) }
  #s <- 1:10

  ini = 2
  for(i in ini:D) {
    x1 = cities$x[s[i:length(s)]]
    y1 = cities$y[s[i:length(s)]]
    d  = sqrt((x1 - cities$x[s[i-1]])^2 + (y1 - cities$y[s[i-1]])^2)
    
    j <- which.min(d) + i - 1
    #cat(j, "\n")
    #cat(s[j], "\n")
    aux = s[i]
    s[i] = s[j]
    s[j] = aux
    
    #cat(i, "\n")
  }

  s
}

greddysolsec <- function(s = NA, attach_left = FALSE) {
 
  if( is.na(s) ) { s <- 1:nrow(cities) }
  #s <- 1:10
  D <- length(s)
  ini = 2
  invalid = c()
  for(i in ini:D) {
    x1 = cities$x[s[i:D]]
    y1 = cities$y[s[i:D]]
    found = FALSE
    d  = sqrt((x1 - cities$x[s[i-1]])^2 + (y1 - cities$y[s[i-1]])^2)
    while( !found ) {
      d[invalid]  = 2*max(d)
      
      j <- which.min(d) + i - 1
      
      if ( edges_coinciding( c(s[i-1], s[j]) ) != 0 ) {
        invalid = c(invalid,  which.min(d))
      } else {
        found = TRUE
      }
    }

    cat(i, "\n")
    invalid=c()
    aux = s[i]
    s[i] = s[j]
    s[j] = aux
    
  }
  
  s
}

greddysolprop <- function(s = NA, attach_left = FALSE) {
  if( is.na(s) ) { s <- 1:nrow(cities) }
  #s <- 1:10
  D <- length(s)
  ini = 2
  for(i in ini:D) {
    x1 = cities$x[s[i:D]]
    y1 = cities$y[s[i:D]]
    d  = sqrt((x1 - cities$x[s[i-1]])^2 + (y1 - cities$y[s[i-1]])^2)
    df = data.frame(dist=d, id=1:(D-i+1))
    df = df[order(df$dist),]
    
    c = ceiling(dist_exp(runif(1,0,0.99))*dim(df)[1])
    c = min(c, ceiling(dim(df)[1] * 0.005))
    #cat("C:", c, "\n")
    j <- df[c,]$id + i -1
    #cat(j, "\n")
    #cat(s[j], "\n")
    aux = s[i]
    s[i] = s[j]
    s[j] = aux
    
    #cat(i, "\n")
  }
  
  s
  
}

greddysol_directional <- function(s = NA, div = 1000, dir = "H") {
  minx = min(cities$x)
  miny = min(cities$y)
  maxx = max(cities$x)
  maxy = max(cities$y)
  offset = ifelse(dir == "H", (maxy - miny )/div, (maxx - minx )/div)
  
  s = c()
  dir2 = TRUE
  for(i in 0:(div-1)) {
    if(dir == "H") {
      l = (miny + offset*i)
      h =  (miny + offset*(i+1))
      
      if( h >= offset*div) { ss = which(l <= cities$y &  cities$y <= h) }
      else {ss = which(l <= cities$y &  cities$y < h)}
      
      #cat("h:", h)
      
      lx = ifelse(dir2, min(cities$x[ss]), max(cities$x[ss]))
      dir2 = !dir2
      left = which(cities$x[ss] == lx )
      ss = c(ss[left], ss[ss != ss[left]])
      
      if( length(ss) > 1 ) { ss = greddysol(ss, attach_left = TRUE) }
      
      
      s = c(s, ss)
      #cat(i, "\n")
    }
  }
  
  s

}

find_almost_cycle <- function(sol, n) {
  D = length(sol)
  fmax = 0
  imax = 1
  dist = getdistances(sol)
  for(i in 1:(D-n-1)) {
    f = subfitness(sol[i:(i+n)]) * (1/getdistance(sol, i, i + n)) #sum(dist[i:(i+n)]) #
    
    if(f > fmax ) {
      fmax = f
      imax = i
    }
    
  }
  
  cand = sol[(imax):(imax+n)]
  
  cand
}

# Return the longer subtour of n cities
# Parameters:
#   sol: a solution for the tsp
#   n: number of cities
max_subtour <- function(sol, n) {
  D = length(sol)
  fmax = 0
  imax = 1
  dist = getdistances(sol)
  for(i in 1:(D-n-1)) {
    f = subfitness(sol[i:(i+n)]) * (1/getdistance(sol, i, i + n)) #sum(dist[i:(i+n)]) #
    
    if(f > fmax ) {
      fmax = f
      imax = i
    }
    
  }
  
  cand = sol[(imax):(imax+n)]
  
  
  for( c in cand ) {
    d = getdistancesto(sol, which(sol == c))
    mind = min(d[d>0])
    p2 = which(d>0 & d<mind*2)
    
    for(c2 in sol[p2]) {
      
      if ( edges_coinciding(c(c, c2)) == 0 ) {
        s2 = sol[sol!=c]
        j = which(sol==c2)
        
        sol = c(s2[1:j], c, s2[(j+1):(D-1)])
        break
      }
    }
  }
  
  sol
  
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

getdistances <- function(sol) {
  x1 <- cities$x[sol[1:length(sol)-1]]
  y1 <- cities$y[sol[1:length(sol)-1]]
  x2 <- cities$x[sol[2:length(sol)]]
  y2 <- cities$y[sol[2:length(sol)]]
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

getdistancesto <- function(sol, index) {
  x1 <- rep(cities$x[sol[index]], length(sol))
  y1 <- rep(cities$y[sol[index]], length(sol))
  x2 <- cities$x[sol[1:length(sol)]]
  y2 <- cities$y[sol[1:length(sol)]]
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

getdistance <- function(sol, c1, c2 = NA) {
  x1 <- cities$x[sol[c1]]
  y1 <- cities$y[sol[c1]]
  c2 = ifelse(is.na(c2) == TRUE, c1+1, c2)
  x2 <- cities$x[sol[c2]]
  y2 <- cities$y[sol[c2]]
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

validate_opt <- function(opt, ...) {
  
  sol = randomsol()
  fsol = subfitness(sol)
  for(i in 1:100) {
    
    n = opt(sol,...)
    
    r = (fsol + n[length(n)] - subfitness(n[1:length(n)-1])) < 0.00001
    
    if(r == FALSE) {return(FALSE)}
  }
  
  TRUE
}

swap <- function(par, i, j) {
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  par
}

neighbor <- function(par, distance, lower,upper) {
  D=length(par) # dimension
  
  #interchange
  i <- sample(1:D, 1)
  j <- ifelse(i==D,1,i+1)
  
  inc = ceiling((j - i) * distance(runif(1,0,1)))
  inc = ifelse(inc > 0, inc, 1)
  inc = ifelse(inc > D - i, D - i, inc)
  j = i + inc
 
  #j <- sample(1:D, 1)
  
  inc = getInterDistances(par, i) + getInterDistances(par, j)
  
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  inc = getInterDistances(par, i) + getInterDistances(par, j) - inc
  
  
  list(sol=par, inc=inc, move=c(i,j))
}

twoopt <- function(par, distance, lower, upper, i = NA, j=NA) {
  D=length(par) # dimension
  
  #interchange
  if( is.na(i) || is.na(j)) {
    n1 <- sample(1:D, 1)
    n2 <- sample(1:D, 1)
    i = min(n1,n2)
    j = max(n1,n2)
    
    
    inc = ceiling((j - i) * distance(runif(1,0,1)))
    inc = ifelse(inc > 0, inc, 1)
    inc = ifelse(inc > D - i, D - i, inc)
    j = i + inc
  }
  inc = getInterDistances(par, i) + getInterDistances(par, j)
  
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  if( i+1 < j-1 ) {
    par = c(par[1:i], rev(par[(i+1):(j-1)]), par[j:length(par)])
  }
  
  inc = getInterDistances(par, i) + getInterDistances(par, j) - inc
  
  #cat("i:", i," j:", j, " D:", length(par), " inc:", inc, "\n")
  
  list(sol=par, inc=inc, move=c(i,j))
}

reduce_max_dist <- function(par , distance, lower, upper) {
  D=length(par) # dimension
  n1 = which.max(getdistances(par))

  inc = ceiling(
    ifelse(runif(1,0,1) > 0.5,
             sample(1:(D - n1), 1), 
             sample((-1:-n1), 1)) * 
           distance(runif(1,0,1)))
  
  n2 = n1 + inc
  n2 = ifelse(n2 < 1,1,n2)
  n2 = ifelse(n2 > D,D,n2)
  
  i = min(n1,n2)
  j = max(n1,n2)
  
  inc = getInterDistances(par, i) + getInterDistances(par, j)
  par = swap(par, i, j)
  
  if( i+1 < j-1 ) {
    par = c(par[1:i], rev(par[(i+1):(j-1)]), par[j:length(par)])
  }
  
  inc = getInterDistances(par, i) + getInterDistances(par, j) - inc
  
  
  
  list(sol=par, inc=inc, move=c(i,j))
}

reduce_max_dist3 <- function(par , distance, lower, upper) {
  D=length(par) # dimension
  dist = getdistances(par)
  df = data.frame(dist=dist, id=1:(D-1))
  df = df[order(df$dist),]
  n = df[ceiling(dist_exp(runif(1,0,1))*D),]$id
  
  
  d = getdistancesto(par, n)
  mind = min(d[d>0])
  p2 = which(d>0 & d<mind*3)
  
  if( length(p2) > 0 ) {
  
    c1 = par[n]
    c2 = par[sample(p2,1)]
    s2 = par[par!=c1]
    j = which(par==c2)
    
    inc = getInterDistances(par, n) + getdistance(par, j, j+1)
    s2 = c(s2[1:j], c1, s2[(j+1):(D-1)])
    inc = getInterDistances(par, j) + getdistance(par, n, n+1) - inc
    
    imp = subfitness(s2)-subfitness(par)
    if( is.na(imp) ) {imp = 0; s2 = par}
    
  }
  else {
    s2 = par
    inc = 0
    imp = 0
    j = 0
    n = 0
  }
  list(par=s2, inc=imp, move=c(n,j), error=(inc))
}

segment_length <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

angle <- function(p2, p1, p3) {
  p12 = segment_length(p1,p2)
  p13 = segment_length(p1,p3)
  p23 = segment_length(p2,p3)
  
  acos((p12^2 + p13^2 - p23^2) / (2 * p12 * p13))
}

city_angle <- function(p2, p1, p3) {
  angle(cities[p2,], cities[p1,], cities[p3,])
}

angles <- function(sol) {
  D = length(sol)
  data = list(p2=sol[1:(D-2)], p1=sol[2:(D-1)], p3=sol[3:D])
  data$angle = city_angle(data$p2, data$p1, data$p3)
  data$angle[is.na(data$angle)] = 0
  c(Inf, data$angle)
} 

reduce_max_dist2 <- function(par , distance, lower, upper) {
  D = length(par)
  max = ceiling(distance(runif(1,0,1))*D)

  d1 = getdistancesto(par, max)
  d1[d1==0] = Inf
  d2 = getdistancesto(par[2:D], max+1)
  d2[d2==0] = Inf
  d = d1[1:(D-1)] + d2
  cand = which.min(d)
  
  i = min(max+1, cand)
  j = max(max+1, cand)
  j = ifelse(i==j,j+2,j)
  

  r = twoopt(par, distance, i=i, j=j)
  
  if(is.na(r$inc)) {
    r$inc = 0
    r$sol = par
  }
  
  r
}
#show_sol(par, par[c(max, max+1, cand, cand+1)])

crossover <- function(p1, p2) {
  D = length(p1)
  c1 = ceiling(runif(1,0,D)) 
  
  o1 = c(p1[1:(i-1)], p2[!(p1[1:(i-1)] %in% p2)])
  o2 = c(p2[1:(i-1)], p1[!(p2[1:(i-1)] %in% p1)])
  
  list(o1=o1, o2=o2)
}

dist_exp <- function(x) { 
  (exp(1) ^ (x*10)) / 20000
}

dist_iden <- function(x) { 
  1
}

fname <- function(y) print(match.call()[2])

sneighbor <- function(par,lower,upper) {
  D=length(par) # dimension
  
  #interchange
  i <- sample(1:D, 1)
  j <- ifelse(i==D,1,i+1)
  
  inc = getInterDistances(par, i) + getInterDistances(par, j)
  
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  inc = getInterDistances(par, i) + getInterDistances(par, j) - inc
  
  par
}

init <- function(type, sol= NA) {
  
  if (type == "greddy") {
    load("greddy.RData")
    sol = greddy_sol 
  }
  else if (type == "greddy second") {
    load("greddy_second.RData")
    sol = greddy_second
  }
  else if (type == "random") {
    sol <- randomsol()
  }
  else if (type == "sol") {
    sol = sol
  }
  else if (type == "second") {
    #load("heuristic/santa_7876507.2183248.RData")
    #sol = best
    load("results/second/hill_d/results/santa_7820688.RData")
    sol = result$sol
  }
  else if (type == "greddy_h") {
    load("greddy_h.RData")
    sol = result$sol
  }
  else {
    load("results/tabu/santa_6351641.RData")
    sol = result$sol
  }
  
  sol
}

show_sol <- function(sol, highlight = NA, colour="black", comparative_sol=NA) {
  p <- ggplot(cities[sol,], aes(x=x, y=y)) 
  if( !is.na(comparative_sol)) {
    p <- p + geom_path(data=cities[comparative_sol,], colour="red")
  }
  
  p <- p + geom_point(size = 1, colour=colour)  + geom_path(colour=colour) 
  if( !is.list(highlight)) {
    p <- p + geom_point(data=cities[highlight,], colour="green")
  }
  
 
  p <- p +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  p <- p +  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  p <- p +  scale_y_continuous(name="") + scale_x_continuous(name="")  
  #a = angles(sol)
  #p <- p +  geom_point(data=cities[sol[which(a < 0.9)],], colour="red")
  p
}


perturbation <- function(sol) {
  for(i in 1:2) {
    move = select(opnames, rep(1, length(opnames)))
    move = op[which(opnames==move)][[1]]
    move_control = select(distnames, rep(1, length(distnames)))
    move_control = dist[which(distnames==move_control)][[1]]
    
    sol = move(sol, move_control)$sol
  }
  
  sol
}

#evolutive parameters
scorefactor = 1
select = function(values, scores) {
  ac = cumsum(scores/sum(scores))
  
  values[ac >= runif(1,0,1)][1]
}

updatescore <- function(scores, selector, value, improving=TRUE, minscore=1) {
  sign = ifelse(improving, 1, -1)
  if (sign==-1) {scorefactor = scorefactor*0.5}
  max = min(sum(scores) * 0.5, 100)
  scores[which(selector==value)] <- min(max(scores[which(selector==value)] + scorefactor*sign, minscore), max)
  scores
}


#trace functions
updatetrace <- function(trace, it, algo, move, move_control, eval, tmax, T, N, L, alpha, init_fitness, iterations, total_eval, pmutation=NA, pcrossover=NA) {
  trace$it = c(trace$it, it)
  trace$alg = c(trace$alg, algo)
  trace$move = c(trace$move, move)
  trace$move_control = c(trace$move_control, move_control)
  trace$eval = c(trace$eval, eval)
  trace$tmax = c(trace$tmax, tmax)
  trace$T = c(trace$T, T)
  trace$N = c(trace$N, N)
  trace$L = c(trace$L, L)
  trace$alpha = c(trace$alpha, alpha)
  trace$init_fitness = c(trace$init_fitness, init_fitness)
  trace$iterations = c(trace$iterations, iterations)
  trace$total_eval = c(trace$total_eval, total_eval)
  
  if(!is.na(pmutation)) {
    if( is.null(trace$pmutation)) {
      trace$pmutation = rep(NA, length(trace$it)-1)
    }
    
    trace$pmutation = c(trace$pmutation, pmutation)
  }
  
  if(!is.na(pcrossover)) {
    if( is.null(trace$pcrossover)) {
      trace$pcrossover = rep(NA, length(trace$it)-1)
    }
    
    trace$pcrossover = c(trace$pcrossover, pcrossover)
  }
  
  trace
}

savetrace <- function(trace, path="iterate_search.csv") {
  if( !is.null(trace$pcrossover)) {
    t = data.frame(it=trace$it, alg=trace$alg, init_fitness=trace$init_fitness, eval=trace$eval, 
                   move=trace$move, move_control=trace$move_control, T=trace$T,
                   N=trace$N, L=trace$L, alpha=trace$alpha, tmax=trace$tmax, 
                   iterations=trace$iterations, total_eval=trace$total_eval,
                   pmutation=trace$pmutation, pcrossover=trace$pcrossover)
  }
  else {
    t = data.frame(it=trace$it, alg=trace$alg, init_fitness=trace$init_fitness, eval=trace$eval, 
                   move=trace$move, move_control=trace$move_control, T=trace$T,
                   N=trace$N, L=trace$L, alpha=trace$alpha, tmax=trace$tmax, 
                   iterations=trace$iterations, total_eval=trace$total_eval )
  }
  
  write.csv(t, file=path)
}

create_optional <- function(sol){
  D = length(sol)
  
  unlist(lapply(((1:(D/6))*6)-5, function(i) { c(sol[i], sol[i+2],sol[i+5],sol[i+3],sol[i+1],sol[i+4]) }))
}

rare <- function(sol){
  D = length(sol)
  
  unlist(lapply(1:(D/6), function(i) { c(sol[i], sol[i+2],sol[i+4],sol[i+1],sol[i+3],sol[i+5]) }))
}

optional_fitness <- function(sol) {
  D = length(sol)
  n = edges_coinciding(sol)
  s = subfitness(sol) 
  s = s + (s/D)*n*3
  s
}

optional_opt <- function(par, dist,  lower, upper) {
  r = twoopt(par, dist, NA, NA)
  
  of = optional_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=optional_fitness(r$sol)-of + penalty, move=r$move)
}

optional_angle_opt <- function(par, dist,  lower, upper) {
  r = angle_opt(par, dist, NA, NA)
  
  of = optional_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=optional_fitness(r$sol)-of + penalty, move=r$move)
}



optional_reduce_max_dist3_opt <- function(par, dist,  lower, upper) {
  r = reduce_max_dist3(par, dist, NA, NA)
  
  of = optional_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$par, inc=optional_fitness(r$par)-of + penalty, move=r$move)
}

optional_near_opt <- function(par, dist,  lower, upper) {
  r = near_opt(par, dist, NA, NA)
  
  of = optional_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=optional_fitness(r$sol)--of + penalty, move=r$move)
}

optional_reduce_max_distance2_opt <- function(par, dist,  lower, upper) {
  r = reduce_max_dist2(par, dist, NA, NA)
  
  of = optional_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=optional_fitness(r$sol)--of + penalty, move=r$move)
}

edges_coinciding <- function(par, show_edges =FALSE) {
  D = length(par)
 
  DAUX = as.numeric(max(c(par,other_sol)))
  #dfos = c(other_sol[1:(DO-1)], rev(other_sol[1:(DO-1)])) * DAUX + c(other_sol[2:DO], rev(other_sol[2:DO]))
  cand = which(other_sol %in% par)
  fos = c(dfos[cand])
  #fos = dfos
  #dfpar = data.frame(l=par[1:(D-1)], r=par[2:D])
  dfpar = c(par[1:(D-1)] *DAUX   + par[2:D], par[2:(D)] *DAUX + par[1:(D-1)] )
  
  r = fos %in% dfpar
  
  if(show_edges ) {
    which(r==TRUE)
  }
  else {
    length(r[r==TRUE])
  }
 
}


#x = (0:100) / 100
#base <- qplot(x, geom = "identity")
#base + stat_function(fun = dist_change, colour = "red")


angle_opt = function(sol, distance, lower, upper) {
  D = length(sol)
  a = angles(sol)
  cand = which(a<1.2)
  n = sample(cand, 1)
  d = getdistancesto(sol, n)
  mind = min(d[d>0])
  p2 = which(d>0 & d<mind*4)
  
  
  c1 = sol[n-1]
  c2 = sol[sample(p2,1)]
  s2 = sol[sol!=c1]
  j = which(sol==c2)
  
  inc = getInterDistances(sol, n) + getdistance(sol, j, j+1)
  s2 = c(s2[1:j], c1, s2[(j+1):(D-1)])
  inc = getInterDistances(sol, j) + getdistance(sol, n, n+1) - inc

  imp = subfitness(s2)-subfitness(sol)
  if( is.na(imp) ) {imp = 0; s2 = sol}
  list(sol=s2, inc=imp, move=c(n,j), error=(inc))
}

near_opt = function(sol, distance, lower, upper) {
  D = length(sol)
  n = sample(1:D, 1)
  d = getdistancesto(sol, n)
  mind = min(d[d>0])
  p2 = which(d>0 & d<mind*3)
  
  
  c1 = sol[n]
  c2 = sol[sample(p2,1)]
  s2 = sol[sol!=c1]
  j = which(sol==c2)
  
  inc = getInterDistances(sol, n) + getdistance(sol, j, j+1)
  s2 = c(s2[1:j], c1, s2[(j+1):(D-1)])
  inc = getInterDistances(sol, j) + getdistance(sol, n, n+1) - inc
  
  imp = subfitness(s2)-subfitness(sol)
  if( is.na(imp) ) {imp = 0; s2 = sol}
  list(sol=s2, inc=imp, move=c(n,j), error=(inc))
}

intersection <- function(p0, p1, p2, p3)
{
  s1_x = p1$x - p0$x
  s1_y = p1$y - p0$y
  s2_x = p3$x - p2$x     
  s2_y = p3$y - p2$y
  
  s = (-s1_y * (p0$x - p2$x) + s1_x * (p0$y - p2$y)) / (-s2_x * s1_y + s1_x * s2_y)
  t = ( s2_x * (p0$y - p2$y) - s2_y * (p0$x - p2$x)) / (-s2_x * s1_y + s1_x * s2_y)
  
  return(s >= 0 && s <= 1 && t >= 0 && t <= 1)

}

inter_opt <- function(sol, distance, lower, upper) {
  D = length(sol)
  for(i in 1:(D-4)) {
    if((i %% 1000) == 0) {cat("i:", i, "\n")}
    d = getdistancesto(sol, n)
    mind = min(d[d>0])
    near = which(d>0 & d<mind*2)
    near = near[near!=D & near != i]
    for(n in near) {
      p0 = cities[sol[i],]
      p1 = cities[sol[i+1],]
      p2 = cities[sol[n],]
      p3 = cities[sol[n+1],]
      
      int = intersection(p0,p1,p2,p3)
      
      if( is.na(int)) {cat("NA:", i, ",", n, "\n") }
      if(int == TRUE) {cat("OK:", i, ",", n, "\n")}
    }
    
  }
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}