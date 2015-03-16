setwd("~/Documentos/projets/kaggle/santa_tsp")
source("santa.r")
source("hill.r")
source("clusterize.R")
set.seed(proc.time()[3])

control = list(itmax=10000,REPORT=0,TRACE=0, fn = subfitness, clusters_size=c(1, 1))
#pop = c(0, 0, 0, 0, 0, 1, 1, 1, 1)
pop = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r = clusterize(init_sol=sample(init("greddy_h")[1:20]), hclimbing, control,
               pop, clu_method="linear", sol_path =NA, second_sol=NA, trace_file=NA)

