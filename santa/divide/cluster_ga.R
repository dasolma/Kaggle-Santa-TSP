setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")
source("alg/ga.R")
source("divide/clusterize.R")
set.seed(proc.time()[3])

control = list(maxiter=1000, itmax=1000,REPORT=0,TRACE=0, fn = fitness, pmutation = 0.4, pcrossover=0.2, popsize=50, clusters_size=c(150, 150))
#pop = c(0, 0, 0, 0, 0, 1, 1, 1, 1)
pop = c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r = clusterize(init_sol=init("greddy"), gen, control,
               pop, clu_method="linear", sol_path ="final/hill/results/", second_sol=NA, trace_file="final/hill/trace.csv")
