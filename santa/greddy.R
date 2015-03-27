setwd("~/Documentos/projets/kaggle/Kaggle-Santa-TSP")
source("santa/santa.R")

cities <- read.csv("santa_cities.csv")
sol <- greddysol()

save(sol, file="greddy.RData")

load("greddy.RData")
