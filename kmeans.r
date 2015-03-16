setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
source("santa_hill.r")

clusters <- kmeans(cities[,2:3],80)

result = c()
for(i in 1:80) {
  SET <- clusters$cluster==i
  sol <- as.numeric(rownames(cities))[SET]
  
  sol = greddysol(sol)
  r = santa_hill("sol", move=twoopt, move_control=dist_exp, list(maxit=100000,REPORT=100,TRACE=0, SAVE=FALSE), sol=sol)
  
  result = c(result, r$sol)
}

subfitness(result)
show_sol(result)

save(result, file="kmeans_hill_7767374.RData")