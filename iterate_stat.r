library("data.table")
setwd("~/Documentos/projets/kaggle/santa_tsp")

it = data.table(read.csv("iterate.csv"))
it$improve = (it$init_fitness-it$eval) / it$init_fitness
it = it[it$improve > 0]
r = it[, list(min=min(improve), mean=mean(improve), sd=sd(improve)), by=list(alg,init)]

ggplot(r, aes(alg, mean, fill=init)) + geom_bar(position="dodge", stat="identity")


r = it[alg=='tabu', list(min=min(improve), mean=mean(improve), sd=sd(improve)), by=list(L,init)]

ggplot(r, aes(L, min, fill=init)) + geom_bar(position="dodge", stat="identity")