setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")
source("alg/tabu.R")
source("alg/hill.R")
source("divide/clusterize.R")
seed = proc.time()[3] 
set.seed(seed)


other_sol <- init("optima")
DO = length(other_sol)
DAUX = as.numeric(max(c(other_sol)))

dfos = c(other_sol[1:(DO-1)] * DAUX)  + c(other_sol[2:DO])
control = list(itmax=1000,REPORT=0,TRACE=0, fn = second_fitness, clusters_size=c(100, 50))
pop = c(0, 0, 0, 0, 0, 1, 0, 0, 0)
r = clusterize(init_sol=init("second"), hclimbing, control,
               pop, clu_method="linear", sol_path ="second/hill_d/results/", 
               second_sol=init("optima"), 
               trace_file="second/hill_d/trace.csv")



control = list(itmax=1000,REPORT=0,TRACE=0, L=20, N=10, fn = optional_fitness, clusters_size=c(100, 200))

#r = list(sol = init("second") )
r = clusterize(init_sol=init("second"), tabu, control,
               pop, clu_method="linear", sol_path ="second/tabu/results/",
               second_sol=init("optima"), 
               trace_file="second/tabu/trace.csv", 
               trace_function=function(control, trace) { 
                  updatetrace(trace,  control$totalit, "tabu", control$move, "dist_exp", 
                             control$eval, control$tmax, NA, control$N, control$L, NA, control$init_eval, control$it,
                             control$total_eval) })


control = list(itmax=10000,REPORT=0,TRACE=0, L=100, N=10, fn = optional_fitness, clusters_size=c(1000, 2000))

#r = list(sol = init("second") )
r = clusterize(init_sol=r$sol, tabu, control,
               pop, clu_method="linear", sol_path ="second/tabu1000it/results/",
               second_sol=init("optima"), 
               trace_file="second/tabu1000it/trace.csv", 
               trace_function=function(control, trace) { 
                 updatetrace(trace,  control$totalit, "tabu", control$move, "dist_exp", 
                             control$eval, control$tmax, NA, control$N, control$L, NA, control$init_eval, control$it,
                             control$total_eval) })
