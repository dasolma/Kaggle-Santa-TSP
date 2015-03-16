ittrace = read.csv("iterate_search.csv")


d = length(result$sol)

#m = matrix(nrow=d, ncol=d)

ittrace = ittrace[ittrace$init_fitness != ittrace$eval,]

#tend improving
ittrace$improve = ittrace$init_fitness - ittrace$eval
p <- ggplot(ittrace, aes(x=it,y=eval)) 
p +  geom_point(aes(colour=alg)) 
  
#comparative alg
p <- ggplot(ittrace, aes(alg)) 
p +  geom_bar(aes(fill=alg)) 

#comparative operator
p <- ggplot(ittrace, aes(move)) 
p +  geom_bar(aes(fill=move)) 

#L 
p <- ggplot(ittrace[!is.na(ittrace$L),], aes(as.factor(L))) 
p +  geom_bar(aes(fill=L)) 

#L 
p <- ggplot(ittrace[!is.na(ittrace$move_control),], aes(as.factor(move_control))) 
p +  geom_bar(aes(fill=move_control)) 

#N = 10


