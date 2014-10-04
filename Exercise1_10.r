num.flips <- 10

ten.flips <- function(){
  flips <- sample(c(0,1), size = num.flips, replace = TRUE)
  mean(flips)
}

num.runs <- 1000

run.experiment <- function(){
  runs <- replicate(num.runs, ten.flips())
  
  c(first.run = runs[1],
    random.run = runs[sample(num.runs, size = 1)],
    min.run = min(runs)
  )
}

num.experiments <- 1e4

experiments <- sapply(1:num.experiments, function(i){
  # print an update every 1000 experiments
  if (i %% 1000 == 1) print(paste("running experiment", i))
  run.experiment()
})

hoeffding <- function(epsilon, N){
  2*exp(-2*(epsilon^2)*N)
}

epsilons <- (0:5)/10
hoeffding.values <- sapply(epsilons, function(epsilon){ hoeffding(epsilon, num.flips)})

dists.first <- abs(experiments[1,] - 0.5)


hist(experiments[1,])
hist(experiments[2,])
hist(experiments[3,])

plot.probability.estimates <- function(nu.values){
  dists <- abs(nu.values - 0.5)
  # multiply by negative one to adjust for stepfun showing < epsilon, while we want > epsilon
  plot.stepfun(-dists)
  lines(-epsilons, hoeffding.values, col="red")
}

plot.probability.estimates(experiments[1,])
plot.probability.estimates(experiments[2,])
plot.probability.estimates(experiments[3,])

