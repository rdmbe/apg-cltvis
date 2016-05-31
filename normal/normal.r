norm <- function(n,mean,sd,nos){
  for(i in 1:nos){
    mean[i] = mean(rnorm(n = n,mean = mean,sd = sd))
  }
  hist(mean , main = paste("Histogram distribusi"), prob = T)
  lines(density(mean))
  lines(x = sort(mean), y = dnorm(sort(mean), mean(mean), sd(mean)), col = "red")
}
