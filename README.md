# Kaggle Santa TSP

![Sampling distributions from a jar](santa.png)

---
# Kaggle Santa TSP

* It's a big problem: **150.000** chimneys

* No only found a path

  - Found two **disjoint** paths: *If one of your paths contains an edge from A to B, your other path must not contain an edge from A to B or from B to A (either order still counts as using that edge)*.
  
  - The final score will be the larger of the two distances.
  
* Strategy:

  1. Find a simple solution, better as possible.

  2. Find the second solution respecting the constraints.

* Lower boundary (sum of the distance of each city to the nearest): **5.405.982**

---
# Use library vs develop it yourself

* In big problems the performance is very important

* The existing libraries execute the fitness function after to apply the operator (no estimate the change)

* I developed local search, tabu search and simulate annealing **to estimate that change**

* I developed ant colony algorithm because **I don't found any implementation** in R

---
# Random solution

![Random solution](images/random.png)

* Aprox. 1.290.000.000

---
# Greddy solution

![Greddy solution](images/greddy.png)

* Fitness: 7.137.517

---
# Kmeans solution

* 80 clusters

![Solution using kmeans](images/kmeans.png)

* Fitness: 7.767.374

---
# First attempt

* Try solve to the complete problem with local search using the two-opt operator from the greddy solution.

![Solution using hill climbing from the greddy solution](images/hill_greddy.png)

* Fitness: 6.804.530

* The greddy heuristic creates **path-crossings**, some of them are dificult to resolve because involve to many chimneys. 

---
# My Greddy solution

![Horizontal greddy solution](images/greddy_h.png)

* Fitness: 17.938.005

* There isn't path-crossings! :)

* Better to comparing algorithms

---
# Operators

* neighbour, 2-opt

![Neighbour operator](images/neighbour-two-opt.png)

* angle-Opt, near-Opt

![Angle and near operator](images/angle_near_opt.png)

---
# Operators

* reduce_max_dist

![Reduce max distances operators](images/reduce_max_distance_opt.png)

---
# Operators

* 100 executions per algorithm and operator, 100 iterations per execution

![Reduce max distances operators](images/alg_op.png)


---
# Divide and select a good operator

* Deal with the complete problem is unfeasible (locality, fitness calculate, ...) 

* I don't know when is better use each operator

* So, I use the follow strategy:

```javascript
	1. For each subroute of size m of the current solution

	   1.1. Select an operator (the best score have more probability to be selected)

	   1.2. Apply an algorithm a number of interations

	   1.3. If there is improvement increase the score of the operator, in other case decrease it

	2. If had improvement in any subroute return to 1

```

---
# Parameters

![Sann and tabu parameters research](images/metaparameters.png)

---
# Comparing the final results

![Sampling distributions from a jar](images/meta.png)

* Better is tabu: 6.486.473

---
# Comparing the final results

![Sampling distributions from a jar](images/operators.png)

* The algorithms select the operators that get improvings

---
# Using operators 

![Graph of the operators using](images/operators_count.png)

---
# Population algorithms

* In general need more memory and the fitness estimation of the operations are difficult

![Parameters ga](images/ga_parameters.png)

---
# Finding a the second solution

* Create a second solution from the first:

![Sampling distributions from a jar](images/heuristics_second.png)

* New fitness function using naive:
```
		G(s) = F(s) + ( F(s)/D ) * 3e
```
* where:

  - D: length of s
  - e: edges coinciding 

---
#  Finding a the second solution

![Second heuristic solution](images/heuristic_second.png)

* Fitness: 8.066.495

---
# Final solution

![Second heuristic solution](images/final.png)

* Fitness: 7.816.935


