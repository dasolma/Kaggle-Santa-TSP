sol = init("greddy second")
fn = optional_fitness

fbest = fn(sol)
D = length(sol)
ec = edges_coinciding(sol)
while(TRUE) {
  
  sorted_indexes = unlist(as.matrix(sort.int(getdistances(sol), index.return=TRUE, decreasing=TRUE))[2])
  for( i in sorted_indexes ) {
    cat("City:", i, " Edgest coinc:", ec, " Better Fitness:", subfitness(sol),"\n")
    D = length(sol)
    n = i
    d = getdistancesto(sol, n)
    mind = min(d[d>0])
    p2 = which(d>0 & d<mind*20)
    n = 20
    while( length(p2) > 100 ) {
      p2 = which(d>0 & d<mind*n)
      n = n -1
    }
    
    sfbest = fbest
    ssol = sol
    for(p in p2) {
      r = twoopt(sol, dist_exp, i=i, j=p )
      
      #print(fn(r$sol))
      if( fn(r$sol) < sfbest) {
        ssol = r$sol 
        sfbest = fn(r$sol)
        ec = edges_coinciding(r$sol)
      }
      
    }
  }
  
  sol = ssol
  fbest = sfbest
} 
