#######################
#                     #
#  Kelompok           #
#  Gamma dan Uniform  #
#                     #
#######################


plothist <- function(n = 10, nos = 100, shape = 1, rate = 1, dist="gamma") {
  
  xbar = rep(0,nos)
  
  if(dist == "gamma"){
    for(i in 1:nos){
      xbar[i] = mean(rgamma(n, shape = shape, rate = rate))
    }  
  }
  else if(dist == "uniform"){
    for(i in 1:nos){
      xbar[i] = mean(runif(n))
    }
  }
  
  xbar
  hist(xbar , main = paste("Histogram distribusi", dist), prob = T)
  #lines dari distribusi bangkitan
  lines(density(xbar))
  
  #lines distribusi normal
  lines(x = sort(xbar), y = dnorm(sort(xbar), mean(xbar), sd(xbar)), col = "red")
  
}


plothist(10,100, dist = "gamma")

