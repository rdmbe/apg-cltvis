library(tcltk2)
library (tkrplot)

plothist <- function() {
  mean <- 0
  std <- 1
  data1 <- rnorm(n=pos,mean,std)
  hist(data1 , main=paste("Histogram Distribusi Normal, mean = ", mean,", std = ", std, "\nn = ",pos,sep="" ),xlab = "",prob = T)
}

#normal
normal.dist <- function(){
  rata<-0
  mean<-0
  sd<-1
  for(i in 1:pos2){
    mean[i] = mean(rnorm(n = pos,mean = rata,sd = sd))
  }
  hist(mean , main = paste("Histogram Distribusi Normal. mean = ",rata," sd = ",sd,"\nSample size = ",pos,", Number of Samples = ",pos2,sep=""),xlab = "", prob = T)
  if(pos2 > 1)
  {
    lines(density(mean),col = "black", lwd = 2.5)
    lines(x = sort(mean), y = dnorm(sort(mean), mean(mean), sd(mean)), col = "red", lwd = 2.5,lty=2)
  }
}

#gamma
gamma.dist <- function(shape = 1, rate = 1) {
  xbar = rep(0,pos2)
  for(i in 1:pos2){
      xbar[i] = mean(rgamma(pos, shape = shape, rate = rate))
  }  
  
  hist(xbar , main = paste("Histogram Distribusi Gamma. alpha = ",shape," beta = ",rate,"\nSample size = ",pos,", Number of Samples = ",pos2,sep = ""),xlab = "", prob = T)
  if(pos2 > 1)
  {
    #lines dari distribusi bangkitan
    lines(density(xbar),col = "black", lwd = 2.5)
    #lines distribusi normal
    lines(x = sort(xbar), y = dnorm(sort(xbar), mean(xbar), sd(xbar)), col = "red", lwd = 2.5,lty=2)
  }
}

#Beta
beta.dist <- function(alpha=1,beta=1) {
  data2 <- rep(0,pos2)
  for(i in 1:pos2) {
    data1 <- rbeta(n=pos,alpha,beta)
    data2[i] <- mean(data1)
  }
  hist(data2 , main=paste("Histogram Distribusi Beta. alpha = ",alpha," beta = ",beta,"\nSample size = ",pos,", Number of Samples = ",pos2,
                          sep="" ) ,xlab = "",prob=TRUE)
  if(pos2 > 1)
  {
    lines(density(data2), col = "black", lwd = 2.5 )
    lines(x = sort(data2), y = dnorm(sort(data2), mean(data2), sd(data2)), col = "red", lwd = 2.5,lty=2)
  }
}

#uniform
uniform.dist <- function() {
  xbar = rep(0,pos2)
  for(i in 1:pos2){
      xbar[i] = mean(runif(pos))
  }
  
  hist(xbar , main = paste("Histogram Distribusi Uniform.\nSample size = ",pos,", Number of Samples = ",pos2,sep = ""),xlab = "", prob = T)
  if(pos2 > 1)
  {
    #lines dari distribusi bangkitan
    lines(density(xbar),col = "black", lwd = 2.5)
    #lines distribusi normal
    lines(x = sort(xbar), y = dnorm(sort(xbar), mean(xbar), sd(xbar)), col = "red", lwd = 2.5,lty=2)
  }
}

CLTvis <-function() {
  tclRequire("BWidget")
  pos <<- 10
  pos2 <<- 1
  pos3 <<- "Normal"
  winPlot <- tktoplevel()
  tkwm.title(winPlot ,"Central Limit Theorem" )
  setFrame <-tkframe(winPlot ,borderwidth=2)
  
  sliderFramePlot1 <- tkframe(winPlot, borderwidth=2)
  labelText <- tclVar(paste("sample sizes"))
  SliderValue <- tclVar("10")
  SliderValueLabel <- tklabel(sliderFramePlot1, text=labelText)
  tkconfigure (SliderValueLabel, textvariable=SliderValue)
  slider <- tkscale(sliderFramePlot1, from=10, to=100, showvalue=T,variable=SliderValue,resolution=1, orient="horizontal",length=400)
  sliderFramePlot2 <- tkframe(winPlot, borderwidth=2)
  labelText2 <- tclVar(paste("number of samples"))
  SliderValue2 <- tclVar("1")
  SliderValueLabel2 <- tklabel(sliderFramePlot2, text=labelText2)
  tkconfigure (SliderValueLabel2, textvariable=SliderValue2)
  slider2 <- tkscale(sliderFramePlot2, from=1, to=1000, showvalue=T,variable=SliderValue2,resolution=1, orient="horizontal",length=400)
  
  plotFrame <- tkframe(winPlot ,relief="groove",borderwidth=2)
  plotFrame2 <- tkframe(winPlot ,relief="groove",borderwidth=2)
  img <- tkrplot(plotFrame,fun=plothist,1.5,1.5)
  tkgrid(img)

  #meanFrame <- tkframe(setFrame)
  #meanVal <- tclVar(" 0 ")
  #meanField <- ttkentry(meanFrame , width = "5",textvariable = meanVal )
  #stdFrame <- tkframe(setFrame )
  #stdVal <- tclVar(" 1 ")
  #stdField <- ttkentry(stdFrame , width = "5",textvariable = stdVal )
  distribFrame <- tkframe(setFrame)
  distribVal <- c("Normal", "Gamma", "Beta", "Uniform")
  distribSel <- tclVar("Select a distribution...")
  #distribField <- tkwidget(distribFrame, "ComboBox", editable=FALSE, values=distribVal)
  distribField <- ttkcombobox(distribFrame, values=distribVal, textvariable=distribSel, state="readonly")
  onOK <- function(...) {
    dist <- tclvalue(distribSel)
    if (dist == "Normal")
    {
      img <- tkrreplot(img,fun=normal.dist,1.5,1.5)
    } else if(dist == "Gamma")
    {
      img <- tkrreplot(img,fun=gamma.dist,1.5,1.5)
    } else if(dist == "Beta")
    {
      img <- tkrreplot(img,fun=beta.dist,1.5,1.5)
    } else if(dist == "Uniform")
    {
      img <- tkrreplot(img,fun=uniform.dist,1.5,1.5)
    } else {
      tkmessageBox(title="Missing...", message = "Select a distribution!")
    }
  }
  okFrame <- tkframe(setFrame)
  OKbutton <- tkbutton(okFrame , text = "Set", foreground = "darkgreen", width = "12",
                       command = onOK, default = "active", borderwidth = 3)
  
  draw <- function(...){
    pos <<- as.numeric(tclvalue(SliderValue))
    pos2 <<- as.numeric(tclvalue(SliderValue2))
    pos3 <<- as.character(tclvalue(distribSel))
    if (pos3 == "Normal")
    {
      img <- tkrreplot(img,fun=normal.dist,1.5,1.5)
    } else if(pos3 == "Gamma")
    {
      img <- tkrreplot(img,fun=gamma.dist,1.5,1.5)
    } else if(pos3 == "Beta")
    {
      img <- tkrreplot(img,fun=beta.dist,1.5,1.5)
    } else if(pos3 == "Uniform")
    {
      img <- tkrreplot(img,fun=uniform.dist,1.5,1.5)
    }
  }
  tkgrid(img)
  #tkgrid(tklabel(meanFrame , text="Mean:"), meanField , sticky="w")
  #tkgrid(tklabel(stdFrame , text="Standard Deviation:"), stdField , sticky="w")
  #tkgrid(distribFrame, meanFrame , stdFrame , sticky="w")
  tkgrid(tklabel(distribFrame, text="Distribution:"), distribField, sticky="w")
  tkgrid(OKbutton, sticky="w")
  tkgrid(distribFrame, okFrame, sticky="we")
  tkconfigure (slider, command = draw)
  slidertxt <- tklabel(sliderFramePlot1 ,text="Sample size:" )
  tkgrid(slidertxt )
  tkgrid.configure(slidertxt ,sticky="we")
  tkgrid(slider)
  tkconfigure (slider2, command = draw)
  slidertxt2 <- tklabel(sliderFramePlot1 ,text="Number of samples:" )
  tkgrid(slidertxt2 )
  tkgrid.configure(slidertxt2 ,sticky="we")
  tkgrid(slider2)
  tkgrid(plotFrame)
  tkgrid(setFrame)
  tkgrid(sliderFramePlot1)
  tkgrid(plotFrame2)
  tkgrid(sliderFramePlot2)
}
CLTvis()