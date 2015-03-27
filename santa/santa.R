library(foreach)
library(doSNOW)
library(ggplot2)
setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/init.R")

cities <- read.csv("santa_cities.csv")

##Find a subroute with the initial city and final city are near
find_almost_cycle <- function(sol, n) {
  D = length(sol)
  fmax = 0
  imax = 1
  dist = getdistances(sol)
  for(i in 1:(D-n-1)) {
    f = fitness(sol[i:(i+n)]) * (1/getdistance(sol, i, i + n)) #sum(dist[i:(i+n)]) #
    
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
    f = fitness(sol[i:(i+n)]) * (1/getdistance(sol, i, i + n)) #sum(dist[i:(i+n)]) #
    
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



fitness <- function(sol) {
  x1 <- cities$x[sol[1:length(sol)-1]]
  y1 <- cities$y[sol[1:length(sol)-1]]
  x2 <- cities$x[sol[2:length(sol)]]
  y2 <- cities$y[sol[2:length(sol)]]
  sum(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}


###
### Functions to calculate distances
###

getInterDistances <- function(sol, i) {
  d = 0
  D = length(sol)
  if(i > 1 && i < D) { ss = (i-1):(i+1) }
  else {
    if(i>1) { ss = (i-1):i}
    else {ss = (i):(i+1)}
  }
  
  fitness(sol[ss])
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

# Return the distance between two points
segment_length <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

## Swap cities on the i-th and j-th positions of the sol
swap <- function(par, i, j) {
  aux = par[i]
  par[i] = par[j]
  par[j]= aux
  
  par
}

## Return the angle of the segments (p2,p1) and (p1,p2)
angle <- function(p2, p1, p3) {
  p12 = segment_length(p1,p2)
  p13 = segment_length(p1,p3)
  p23 = segment_length(p2,p3)
  
  acos((p12^2 + p13^2 - p23^2) / (2 * p12 * p13))
}

city_angle <- function(p2, p1, p3) {
  angle(cities[p2,], cities[p1,], cities[p3,])
}

## Return the angles of the sol
angles <- function(sol) {
  D = length(sol)
  data = list(p2=sol[1:(D-2)], p1=sol[2:(D-1)], p3=sol[3:D])
  data$angle = city_angle(data$p2, data$p1, data$p3)
  data$angle[is.na(data$angle)] = 0
  c(Inf, data$angle)
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

# Read a initial sol from disk
init <- function(type, sol= NA) {
  
  if (type == "greddy") {
    load("data/greddy.RData")
    sol = greddy_sol 
  }
  else if (type == "greddy second") {
    load("data/greddy_second.RData")
    sol = greddy_second
  }
  else if (type == "random") {
    sol <- randomsol()
  }
  else if (type == "sol") {
    sol = sol
  }
  else if (type == "second") {
    load("data/santa_7820688.RData")
    sol = result$sol
  }
  else if (type == "greddy_h") {
    load("data/greddy_h.RData")
    sol = result$sol
  }
  else if (type == "better") {
    load("data/greddy_h.RData")
    sol = result$sol
  }
  else {
    load("data/santa_6351641.RData")
    sol = result$sol
  }
  
  sol
}

# Plot a sol. 
# Parameters:
#  sol: solution to plot
#  highlight: points to highlight
#  comparative_sol: plot a second sol
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

#
# TRACE FUNCTIONS
#
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


## Count the coinciding edges between two solutions
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

# Check if the edges (p0,p1) and (p2,p3) there is a interception
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

minimun_sol <- function() {
  sum(sapply(1:150000, function(x) { 
    d = getdistancesto(1:150000, x)  
    min(d[d!=0])

    }))
}

source("santa/operators.R")
source("santa/init.R")