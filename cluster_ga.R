setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
source("ga.r")
source("clusterize.R")
set.seed(proc.time()[3])

control = list(maxiter=1000, itmax=1000,REPORT=0,TRACE=0, fn = subfitness, popsize=50, clusters_size=c(150, 150))
#pop = c(0, 0, 0, 0, 0, 1, 1, 1, 1)
pop = c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r = clusterize(init_sol=init("greddy"), gen, control,
               pop, clu_method="linear", sol_path ="final/hill/results/", second_sol=NA, trace_file="final/hill/trace.csv")
