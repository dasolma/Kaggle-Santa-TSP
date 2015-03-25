setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
library(ggplot2)
library(data.table)
stats1 = read.csv("results/final/tabu L100/trace.csv")
stats1$it = stats1$it * stats1$N
stats2 = read.csv("results/final/hill/trace.csv")
stats3 = read.csv("results/final/sannT5/trace.csv")
stats3$alg = rep("sann", length(stats3$alg))
stats3$it = stats3$it * 10

stats = rbind(stats1, stats2, stats3)
factor = max(stats$it)/10
stats$groups = as.integer(stats$it/(factor)) * factor

ggplot(stats[stats$it < 20000000,]) +
  geom_line(aes(x=it, y = (init_fitness-eval), colour = alg)) +
  labs(x="iterations", y="eval")


ggplot(stats1[stats1$it < 20000000,]) +
  geom_point(aes(x=it, y = total_eval, colour = move)) +
  labs(x="iterations", y="eval")

ggplot(stats[stats$alg=='tabu',], aes(move, fill=move)) + geom_bar() +
  facet_wrap(~ groups)


parameters = read.csv("results/parameters_stat.csv")
dtp = data.table(parameters)

dtmean = dtp[, list(min=min(eval), mean=mean(eval), sd=sd(eval) ) , by=alg]

psann = dtp[alg=='sann', list(min=min(eval), mean=mean(eval), sd=sd(eval) ) , by=list(T,alpha)]

ptabu = dtp[alg=='tabu', list(min=min(eval), mean=mean(eval), sd=sd(eval) ) , by=list(N,L)]

ggplot(ptabu, aes(x = as.factor(N), y = as.factor(L))) +
  geom_tile(aes(fill = mean)) +
  theme_bw() + labs(x="Search iterations", y="Tabu list length") +
  ggtitle("Tabu")

ga = read.csv("results/ga/ga_stats.csv")
dtga = data.table(ga)

pga = dtga[, list(min=min(eval), mean=mean(eval), sd=sd(eval) ) , by=list(pmutation,pcrossover)]

ggplot(pga) +
  geom_tile(aes(x=pmutation, y = pcrossover, colour = mean)) +
  labs(x="mutation prob.", y="crossover prob.")


dtalg = dtp[, list(min=min(eval), mean=mean(eval), sd=sd(eval) ) , by=list(alg, move)]
ggplot(dtalg, aes(x = as.factor(alg), y = as.factor(move))) +
  geom_tile(aes(fill = mean)) +
  theme_bw() + labs(x="Algorithm", y="Operator")

