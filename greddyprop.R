setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")



result = list(sol=greddysolprop(1:(dim(cities)[1])))
result$eval = subfitness(result$sol)
sol_path = "greddy_prop/"
save(result, file=paste(sol_path, "santa_", ceiling(result$eval),  ".RData", sep=""))