setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")
source("alg/sann.R")
source("alg/hill.R")
source("divide/clusterize.R")
set.seed(proc.time()[3])

control = list(itmax=100,REPORT=0,TRACE=0, T=100, a=0.95, tmax=10, fn =fitness, clusters_size=c(100, 200, 300, 50, 20))

#pop = c(0, 0, 0, 0, 0, 1, 1, 1, 1)
pop = c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r = clusterize(init_sol=init("greddy_h"), sann, control,
               pop, clu_method="linear", sol_path ="final/sannT100/results/", second_sol=NA, trace_file="final/sannT100/trace.csv", 
               trace_function=function(control, trace) { 
                 updatetrace(trace,  control$totalit, "tabu", control$move, "dist_exp", 
                             control$eval, control$tmax, control$T, NA, NA, control$a, control$init_eval, control$it,
                             control$total_eval) })
