#BACA PENTING!!!
#alpha dan beta nya kita bikin sama dengan 1, tapi sebaiknya bisa diganti2 lewat GUInya
#Fungsi CLT untuk populasi distribusi beta
#Cara1
plothist <- function(samp.size,samp.num) {
  alpha <- 1
  beta <- 1
  i.it <- samp.num/samp.size
  data1 <- rbeta(n=samp.num,alpha,beta)
  data2 <- list()
  for(i in 1:i.it) {
    timing <- list()
    timing[["start"]]<- Sys.time()
    data2.samp <- sample(x = data1,size = samp.size,replace = FALSE)
    d2s.mean <- mean(data2.samp)
    data2[i]<-d2s.mean
    timing[["stop"]]<- Sys.time()
  }
  data2<-as.numeric(data2)
  hist(data2 , main=paste("Histogram,  alpha = ", alpha,", beta = ", beta, ", n = ",samp.size ,
                          sep="" ) ,col="blue" , xlab="", nclass=30, prob=TRUE)
  lines(density(data2), col = "black", lwd = 2.5 )
  lines(x = sort(data2), y = dnorm(sort(data2), mean(data2), sd(data2)), col = "red", lwd = 2.5)
  time <- timing[["stop"]]-timing[["start"]]
  time
}


#Cara2
plothist <- function(samp.size,samp.num) {
  alpha <- 1
  beta <- 1
  i.it <- samp.num/samp.size
  data1 <- rbeta(n=samp.num,alpha,beta)
  data2 <- list()
  for(i in 1:samp.num) {
    timing <- list()
    timing[["start"]]<- Sys.time()
    data1 <- rbeta(n=samp.size,alpha,beta)
    d2s.mean <- mean(data1)
    data2[i]<-d2s.mean
    timing[["stop"]]<- Sys.time()
  }
  data2<-as.numeric(data2)
  hist(data2 , main=paste("Histogram,  alpha = ", alpha,", beta = ", beta, ", n = ",samp.size ,
                          sep="" ) ,col="blue" , xlab="", nclass=30,prob=TRUE)
  lines(density(data2), col = "black", lwd = 2.5 )
  lines(x = sort(data2), y = dnorm(sort(data2), mean(data2), sd(data2)), col = "red", lwd = 2.5)
  time <- timing[["stop"]]-timing[["start"]]
  time
}