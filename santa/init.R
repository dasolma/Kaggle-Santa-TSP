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
