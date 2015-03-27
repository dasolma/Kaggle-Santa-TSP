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
  
  imp = fitness(s2)-fitness(sol)
  if( is.na(imp) ) {imp = 0; s2 = sol}
  list(sol=s2, inc=imp, move=c(n,j), error=(inc))
}

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
  
  imp = fitness(s2)-fitness(sol)
  if( is.na(imp) ) {imp = 0; s2 = sol}
  list(sol=s2, inc=imp, move=c(n,j), error=(inc))
}

crossover <- function(p1, p2) {
  D = length(p1)
  c1 = ceiling(runif(1,0,D)) 
  
  o1 = c(p1[1:(i-1)], p2[!(p1[1:(i-1)] %in% p2)])
  o2 = c(p2[1:(i-1)], p1[!(p2[1:(i-1)] %in% p1)])
  
  list(o1=o1, o2=o2)
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


#
# Modified operators for the second solution searching
#



second_fitness <- function(sol) {
  D = length(sol)
  n = edges_coinciding(sol)
  s = fitness(sol) 
  s = s + (s/D)*n*3
  s
}

second_opt <- function(par, dist,  lower, upper) {
  r = twoopt(par, dist, NA, NA)
  
  of = second_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=second_fitness(r$sol)-of + penalty, move=r$move)
}

second_angle_opt <- function(par, dist,  lower, upper) {
  r = angle_opt(par, dist, NA, NA)
  
  of = second_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=second_fitness(r$sol)-of + penalty, move=r$move)
}



second_reduce_max_dist3_opt <- function(par, dist,  lower, upper) {
  r = reduce_max_dist3(par, dist, NA, NA)
  
  of = second_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$par, inc=second_fitness(r$par)-of + penalty, move=r$move)
}

second_near_opt <- function(par, dist,  lower, upper) {
  r = near_opt(par, dist, NA, NA)
  
  of = second_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=second_fitness(r$sol)--of + penalty, move=r$move)
}

second_reduce_max_distance2_opt <- function(par, dist,  lower, upper) {
  r = reduce_max_dist2(par, dist, NA, NA)
  
  of = second_fitness(par)
  penalty = ifelse(r$move[1] == 1, of * 0.1, 0)
  penalty = penalty + ifelse(r$move[2] == length(par), of * 0.1, 0)
  
  list(sol=r$sol, inc=second_fitness(r$sol)--of + penalty, move=r$move)
}

#
# OTHER FUNCTIONS
#

# Do a perturbation in a solution
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

# return a second solution from the solution gived where
# there isn't common edges
create_second <- function(sol){
  D = length(sol)
  
  unlist(lapply(((1:(D/6))*6)-5, function(i) { c(sol[i], sol[i+2],sol[i+5],sol[i+3],sol[i+1],sol[i+4]) }))
}
