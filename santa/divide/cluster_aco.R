setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")
source("alg/aco.R")
source("divide/clusterize.R")
set.seed(proc.time()[3])

#alpha: pheromone influence
#beta: distance influence
C = list(itmax=10000,REPORT=100,TRACE=0, reduction_rate=0.001, 
         ants = 3, alpha=0.4, beta=0.6, attach_extremes=TRUE,
         fn = fitness, clusters_size=c(3000,3000))


pop = c(1, 1, 1, 1, 1, 0, 0, 0, 0)
opitfactor = rep(1, length(pop))
r = clusterize(init_sol=init("greddy_h"), aco, C,
               pop, clu_method="linear", sol_path ="final/sannT5/results/", second_sol=NA, trace_file="final/sannT5/trace.csv", 
               trace_function=function(control, trace) { 
                 updatetrace(trace,  control$totalit, "tabu", control$move, "dist_exp", 
                             control$eval, control$tmax, control$beta, NA, NA, control$alpha, control$init_eval, control$it,
                             control$total_eval) })