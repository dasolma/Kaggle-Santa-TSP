source("santa_hill.r")


santa_hill("optima", move=twoopt, move_control=dist_exp, list(maxit=10000000,REPORT=100,TRACE=100, SAVE=TRUE))