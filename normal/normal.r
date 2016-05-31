norm <- function(n,mean,sd,nos){
  library(ggplot2)
  means <- rep(NA,nos)
  
  for(i in 1:nos){
    means[i] = mean(rnorm(n = n,mean = mean,sd = sd))
  }
  
  meanss <- as.data.frame(means)
  
  ggplot(data=meanss, aes(x=means)) + geom_histogram(aes(y=..density..,fill=..density..),binwidth = 0.2) + geom_density() +
    stat_function(fun=dnorm,color="red",args=list(mean=mean(means),sd=sd(means)))
  
  #hist(means,main = paste("Histogram distribusi"),breaks=5, prob = T)
  
  #lines(density(means))
  #lines(x = sort(means), y = dnorm(sort(means), mean(means), sd(means)), col = "red")
}
