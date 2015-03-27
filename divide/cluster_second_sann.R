setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")
source("alg/sann.R")
source("alg/hill.R")
source("divide/clusterize.R")
seed = proc.time()[3] 
set.seed(seed)


other_sol <- init("optima")
DO = length(other_sol)
DAUX = as.numeric(max(c(other_sol)))

dfos = c(other_sol[1:(DO-1)] * DAUX)  + c(other_sol[2:DO])
control = list(itmax=1000,REPORT=0,TRACE=0, T=10, tmax=100, fn = second_fitness, clusters_size=c(1000, 5000))
pop = c(0, 0, 0, 0, 0, 1, 0, 0, 0)

r = clusterize(init_sol=init("second"), sann, control,
               pop, clu_method="linear", sol_path ="second/sann/results/",
               second_sol=init("optima"), 
               trace_file="second/sann/trace.csv", 
               trace_function=function(control, trace) { 
                 updatetrace(trace,  control$totalit, "sann", control$move, "dist_exp", 
                             control$eval, control$tmax, control$T, NA, NA, NA, control$init_eval, control$it,
                             control$total_eval) })